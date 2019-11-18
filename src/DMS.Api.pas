{ Documentchain (DMS Core) Example Code https://documentchain.org}

unit DMS.Api;

interface

uses
  System.Classes, System.json, IdHTTP;

const
  cFeedStoreDocInfo = 0.1;  // Transaction fee
  cDMSCoreExeName = 'dms-qt.exe';

type
  TAuditStatus = (auditUnknown,
                  auditOk,
                  auditHint,
                  auditViolation);

  TTransaction = record
    hex, txid, hash: string;
    time, blocktime: TDateTime;
  end;

  TInfoRecord = record
    version: string;
    protocolversion: string;
    blocks: string;
    timeoffset: string;
    connections: string;
    proxy: string;
    difficulty: string;
    testnet: string;
    paytxfee: string;
    relayfee: string;
  end;

  TNetWorkInfoRecord = record
    version: string;
    subversion: string;
    protocolversion: string;
    localservices: string;
    localrelay: string;
    timeoffset: TDateTime;
    connections: string;
    relayfee: string;
    warnings: string;
  end;

  TBlock = class(TObject)
  public
    ajson: string;
    hash: string;
    confirmations: integer;
    strippedsize: integer;
    size: integer;
    weight: integer;
    height: integer;
    version: integer;
    versionHex: string;
    merkleroot: string;
    transactions: tstringlist;
    time, mediantime: TDateTime;
    nonce: int64;
    bits: string;
    difficulty: extended;
    chainwork: string;
    previousblockhash, nextblockhash: string;
  end;

  TNewBlockEvent = procedure(const aBlock: TBlock) of object;
  TBlockCountEvent = procedure(const aBlockCoun: cardinal) of object;

  TBlockchainDocument = record
  private
    // blockchain Data
    FTimeUTC,
    FBlockTimeUTC: TDateTime;
    FHeight,
    FConfirms: integer;
    FBlockHash: string;
    // document data
    FGUID,
    FFileHashMD5,
    FAttrHashMD5,
    FPublisherHashMD5: string;
    FDocDataHex  : string;
  public
    Loaded: boolean;
    procedure FillData(AGUID, AFileHashMD5, AAttrHashMD5, APublisherHashMD5: string); overload;
    procedure FillData(const AHex: string); overload;
    property GUID: string read FGUID;
    property FileHashMD5: string read FFileHashMD5;
    property BlockTimeUTC: TDateTime read FBlockTimeUTC;
    class function CompareDocuments(const ACurrDoc, AStoredDoc: TBlockchainDocument;
                                    AResults: TStrings = nil; AMultiDocMode: boolean = false): TAuditStatus; static;
  end;

  TDocumentchain = class(TComponent)
  strict private
    FOnReady: TNotifyEvent;
    FOnNewBlock: TNewBlockEvent;
    FOnBlockCount: TBlockCountEvent;
    FHTTP: TIdHTTP;
    FJSON: TJsonobject;
    FURL: string;  // http://127.0.0.1:41320
    function Post(const ACommand: string): string;
    function GetResultFromJSON(const AJson: string): string;
    function StoreHex(const AHex: string; const AFee: real; const AMinConfirms: integer): string;
    function FGetDocument(const ATxID: string): TBlockchainDocument;
  private
  public
    constructor Create(Owner: TComponent; const AURL, ARPCUser, ARPCPassword: string);
    destructor Destroy; override;
    function GetJSon(ACommand: string): string;
    function GetString(ACommand: string): string;
    function GetBlockJSON(const aBlockHash: string): string;
    function GetBlockHash(const ABlockNumber: integer): string;
    function GetBlock(const ABlockHash: string): TBlock;
    function GetInfo: TInfoRecord;
    function GetDifficulty: string;
    function GetNetworkInfo: TNetWorkInfoRecord;
    function GetBlockCount: int64;
    function GetRawTransaction(const ATxID: string; AVerbose: boolean = false): string;
    function GetTransaction(const ATxID: string): TTransaction;
    function StoreText(const AText: string; const AFee: real = cFeedStoreDocInfo; const AMinConfirms: integer = 6): string;
    function StoreDocumentInfo(const ADocInfo: TBlockchainDocument;
                               const AFee: real = cFeedStoreDocInfo; const AMinConfirms: integer = 6): string;
    function GetDocument(const ATxID: string): TBlockchainDocument;
    property OnReady: TNotifyEvent read FOnReady write FOnReady;
    property OnNewBlock: TNewBlockEvent read FOnNewBlock write FOnNewBlock;
    property OnBlockCount: TBlockCountEvent read FOnBlockCount write FOnBlockCount;
  end;

implementation

uses
  System.SysUtils, Fmx.Forms, System.DateUtils, System.IOUtils, System.StrUtils,
  Fmx.Dialogs;

const cDocMagicChars     = 'DM$';    // magic chars
      cDocMagicCharsHex  = '444D24';
      cDocDataVersion    = 1;
      cDocDataVersionHex = '0001';   // IntToHex(1)
      cDocTypeAppHex     = '0099';   // app id
      cEmptyMD5Hash      = 'D41D8CD98F00B204E9800998ECF8427E';
      cMaxInputAmount    = 21; // for safety do not take inputs with a higher balance
      cRPCCommandFmt     = '{ "jsonrpc": "1.0", "id":"DMSExposed", "method": "%s", "params": [%s] }';

function String2Hex(const AStr: string): string;
var r: RawByteString;
begin
  r := UTF8Encode(AStr);
  SetLength(result, 2 * Length(r));
  BinToHex(@r[1], PWideChar(result), Length(r)); // * SizeOf(AnsiChar));
end;

function Hex2String(const AHex: string): string;
var r: RawByteString;
begin
  SetLength(r, Length(AHex) div 2);
  HexToBin(PWideChar(AHex), r[1], Length(AHex)); // div SizeOf(AnsiChar));
  result := UTF8ToString(r);
end;

{ TBlockchainDocument }

function CleanDataToken(const ASrc: string): string;
begin
  result := ASrc.Trim;
  result := result.Replace(#13#10, #10, [rfReplaceAll]);
  result := result.Replace(#13, #10, [rfReplaceAll]);
end;

procedure TBlockchainDocument.FillData(AGUID, AFileHashMD5, AAttrHashMD5, APublisherHashMD5: string);
begin
  // check hash length
  if AFileHashMD5.IsEmpty then
    AFileHashMD5 := cEmptyMD5Hash;
  if AFileHashMD5.Length <> 32 then
    raise Exception.Create('Invalid FileHastMD5');
  if AAttrHashMD5.IsEmpty then
    AAttrHashMD5 := cEmptyMD5Hash;
  if AAttrHashMD5.Length <> 32 then
    raise Exception.Create('Invalid AttrHashMD5');
  //
  FGUID := AGUID;
  FFileHashMD5 := AFileHashMD5;
  FAttrHashMD5 := AAttrHashMD5;
  FPublisherHashMD5 := IfThen(APublisherHashMD5<>cEmptyMD5Hash, APublisherHashMD5); // Hash or empty str
  // res str
  FDocDataHex := cDocMagicCharsHex
               + cDocDataVersionHex
               + cDocTypeAppHex
               + FGUID.Replace('{', '').Replace('-', '', [rfReplaceAll]).Replace('}', '')
               + FFileHashMD5
               + FAttrHashMD5
               + FPublisherHashMD5;
end;

{ Blockchain data strukt:
  GUID (compressed), FileHashMD5 and AttrHashMD5 are already Hex data
   0..5  : '444D24' = Hex 'DM$'
   6..9  : '0001'   = Hex Blockchain Data Version 1
  10..13 : '0099'   = Hex app id
  14..45 : GUID (compressed)
  46..77 : FileHashMD5
  78..109: AttrHashMD5
  ab 110 : PublisherHash or empty str }

procedure TBlockchainDocument.FillData(const AHex: string);
begin
  FDocDataHex := AHex;
  // must start with dm$ (lower case)
  if not SameText(FDocDataHex.Substring(0, 6), cDocMagicCharsHex) then
    raise Exception.Create('Invalid format');
  // Blockchain Data Version...
  if FDocDataHex.Substring(6, 4) <> cDocDataVersionHex then
    raise Exception.Create('Unknown format version');
  // GUID: 488ED27B79144F009C4E2806D57569FF => {488ED27B-7914-4F00-9C4E-2806D57569FF}
  FGUID := '{' + FDocDataHex.Substring(14, 32) + '}';
  FGUID := FGUID.Insert(9,  '-');
  FGUID := FGUID.Insert(14, '-');
  FGUID := FGUID.Insert(19, '-');
  FGUID := FGUID.Insert(24, '-');
  FGUID := FGUID.ToUpper;
  StringToGUID(FGUID);
  // hashes
  FFileHashMD5 := FDocDataHex.Substring(46, 32).ToUpper;
  FAttrHashMD5 := FDocDataHex.Substring(78, 32).ToUpper;
  // publisher...
  FPublisherHashMD5 := FDocDataHex.Substring(110, 32).ToUpper; // ca be empty
end;

class function TBlockchainDocument.CompareDocuments(const ACurrDoc, AStoredDoc: TBlockchainDocument;
                                   AResults: TStrings = nil; AMultiDocMode: boolean = false): TAuditStatus;
var LBlockTime: string;
    LPublisherOk: boolean;
begin
  // Publisher war bis .548 als verschlüsselter Text gespeichert
  // ab .549 (OM Data Version 2) wird nur der Hash veröffentlicht
  LPublisherOk := (ACurrDoc.FPublisherHashMD5 = AStoredDoc.FPublisherHashMD5);
  // alles unverändert...
  if  (ACurrDoc.FGUID = AStoredDoc.FGUID)
  and (ACurrDoc.FFileHashMD5 = AStoredDoc.FFileHashMD5)
  and (ACurrDoc.FAttrHashMD5 = AStoredDoc.FAttrHashMD5)
  and LPublisherOk
  then
    result := auditOk
  else begin
    if (ACurrDoc.FGUID <> AStoredDoc.FGUID)
    or (ACurrDoc.FFileHashMD5 <> AStoredDoc.FFileHashMD5)
    then
      result := auditViolation
    else
      result := auditHint;
  end;

  if Assigned(AResults) then begin
    LBlockTime := DateTimeToStr(AStoredDoc.FBlockTimeUTC) + ' UTC'; // neu .544
    // Ergebnis...
    case result of
      auditOk       : AResults.Add(Format('OK. The document has not been changed since %s', [LBlockTime]));
      auditHint     : AResults.Add(Format('File OK, Attribute changed. The document file has not been changed since %s', [LBlockTime]));
      auditViolation: AResults.Add('No confirmation! The document has been modified or it is another document.');
    end;
    AResults.Add('Block timestamp: ' + LBlockTime);
  end;
end;

{ TDocumentchain }

constructor TDocumentchain.Create(Owner: TComponent; const AURL, ARPCUser, ARPCPassword: string);
begin
  inherited Create(Owner);
  FURL := AURL;  // 'http://127.0.0.1:8332'
  FHTTP := TIdHTTP.Create(self);
  FHTTP.Request.Username := ARPCUser;
  FHTTP.Request.Password := ARPCPassword;
  FHTTP.Request.BasicAuthentication := true;
end;

destructor TDocumentchain.Destroy;
begin
  FHTTP.free;
  inherited;
end;

{***** RPC functions *****}

function TDocumentchain.Post(const ACommand: string): string;
var LJsonToSend: TStringStream;
begin
  LJsonToSend := TStringStream.Create(ACommand);
  try
    result := FHTTP.Post(FURL, LJsonToSend);
  finally
    LJsonToSend.Free;
  end;
end;

function TDocumentchain.GetResultFromJSON(const AJson: string): string;
begin
  with TJsonobject.Create do
  try
    if Parse(BytesOf(AJson), 0) > 0 then
      result := GetValue<string>('result');
  finally
    Free;
  end;
end;

function TDocumentchain.GetJSon(ACommand: string): string;
// ACommand Example: { "jsonrpc": "1.0", "id":"DMSExposed", "method": "getblockhash", "params": [10] }
begin
  result := Post(ACommand);
end;

function TDocumentchain.GetString(ACommand: string): string;
// ACommand Example: { "jsonrpc": "1.0", "id":"DMSExposed", "method": "getblockhash", "params": [10] }
begin
  result := GetResultFromJSON(Post(ACommand));
end;

function TDocumentchain.GetBlockHash(const ABlockNumber: integer): string;
begin
  result := GetResultFromJSON
    (Post(Format
    ('{"jsonrpc": "1.0", "id":"DMSExposed", "method": "getblockhash", "params": [%d] }',
    [ABlockNumber])));
end;

function TDocumentchain.GetBlock(const ABlockHash: string): TBlock;
var
  FJSON: TJsonobject;
  json: string;
  aa: tjsonvalue;
  tx: TJSONArray;
  en: TJSONArray.TEnumerator; // Delphi 10.2: TJSONArrayEnumerator;
begin
  json := Post
    (Format('{"jsonrpc": "1.0", "id":"DMSExposed", "method": "getblock", "params": ["%s"] }',
    [ABlockHash]));

  result := TBlock.Create;

  FJSON := TJsonobject.Create;
  if FJSON.Parse(BytesOf(json), 0) > 0 then
  begin
    result.ajson := json;
    aa := FJSON.GetValue('result');
    result.hash := aa.GetValue<string>('hash');
    result.confirmations := aa.GetValue<integer>('confirmations');
    result.strippedsize := aa.GetValue<integer>('strippedsize');
    result.size := aa.GetValue<integer>('size');
    result.weight := aa.GetValue<integer>('weight');
    result.height := aa.GetValue<integer>('height');
    result.version := aa.GetValue<integer>('version');
    result.versionHex := aa.GetValue<string>('versionHex');
    result.merkleroot := aa.GetValue<string>('merkleroot');

    // get transactions
    result.transactions := tstringlist.Create;
    tx := aa.GetValue<TJSONArray>('tx');
    en := tx.GetEnumerator;
    while en.MoveNext do
    begin
      result.transactions.Add(en.GetCurrent.ToString);
    end;

    result.time := UnixToDateTime(aa.GetValue<int64>('time'));
    result.mediantime := UnixToDateTime(aa.GetValue<int64>('mediantime'));
    result.nonce := aa.GetValue<int64>('nonce');
    result.bits := aa.GetValue<string>('bits');
    result.difficulty := aa.GetValue<extended>('difficulty');
    result.chainwork := aa.GetValue<string>('chainwork');

    // genesis block doesnt have prev
    if result.height > 0 then
      result.previousblockhash := aa.GetValue<string>('previousblockhash')
    else
      result.previousblockhash := '';

    result.nextblockhash := aa.GetValue<string>('nextblockhash');
  end;
  FJSON.free;
end;

function TDocumentchain.GetBlockJSON(const ABlockHash: string): string;
begin
  result := Post
    (Format('{"jsonrpc": "1.0", "id":"DMSExposed", "method": "getblock", "params": ["%s"] }',
    [ABlockHash]));
end;

function TDocumentchain.GetBlockCount: int64;
begin
  result := StrToInt
    (GetResultFromJSON
    (Post('{"jsonrpc": "1.0", "id":"DMSExposed", "method": "getblockcount", "params": [] }'))
    );
end;

function TDocumentchain.GetDifficulty: string;
begin
  result := Post('{"jsonrpc": "1.0", "id":"DMSExposed", "method": "getdifficulty", "params": [] }')
end;

function TDocumentchain.GetInfo: TInfoRecord;
var
  ajson: string;
  aa: tjsonvalue;
begin
  ajson := Post('{"jsonrpc": "1.0", "id":"DMSExposed", "method": "getinfo", "params": [] }');
  FJSON := TJsonobject.Create;
  try
    if FJSON.Parse(BytesOf(ajson), 0) > 0 then begin
      aa := FJSON.GetValue('result');
      result.version := aa.GetValue<string>('version');
      result.protocolversion := aa.GetValue<string>('protocolversion');
      result.blocks := aa.GetValue<string>('blocks');
      result.timeoffset := aa.GetValue<string>('timeoffset');
      result.connections := aa.GetValue<string>('connections');
      result.proxy := aa.GetValue<string>('proxy');
      result.difficulty := aa.GetValue<string>('difficulty');
      result.testnet := aa.GetValue<string>('testnet');
      result.paytxfee := aa.GetValue<string>('paytxfee');
      result.relayfee := aa.GetValue<string>('relayfee');
    end;
  finally
    FJSON.free;
  end;
end;

function TDocumentchain.GetNetworkInfo: TNetWorkInfoRecord;
var
  ajson: string;
  aa: tjsonvalue;
begin
  ajson := Post('{"jsonrpc": "1.0", "id":"DMSExposed", "method": "getnetworkinfo", "params": [] }');
  FJSON := TJsonobject.Create;
  try
    if FJSON.Parse(BytesOf(ajson), 0) > 0 then begin
      aa := FJSON.GetValue('result');
      result.version := aa.GetValue<string>('version');
      result.subversion := aa.GetValue<string>('subversion');
      result.protocolversion := aa.GetValue<string>('protocolversion');
      result.localservices := aa.GetValue<string>('localservices');
      result.localrelay := aa.GetValue<string>('localrelay');
      result.timeoffset := UnixToDateTime(aa.GetValue<int64>('timeoffset'));
      result.connections := aa.GetValue<string>('connections');
      result.relayfee := aa.GetValue<string>('relayfee');
      result.warnings := aa.GetValue<string>('warnings');
    end;
  finally
    FJSON.free;
  end;
end;

function TDocumentchain.GetTransaction(const ATxID: string): TTransaction;
var
  LJson: string;
  aa: tjsonvalue;
begin
  LJson := GetRawTransaction(ATxID);
  try
    FJSON := TJsonobject.Create;
    if FJSON.Parse(BytesOf(LJson), 0) > 0 then begin
      aa := FJSON.GetValue('result');
      result.time := UnixToDateTime(aa.GetValue<int64>('time'));
      result.blocktime := UnixToDateTime(aa.GetValue<int64>('blocktime'));
    end;
  finally
    FJSON.free;
  end;
end;

function TDocumentchain.GetRawTransaction(const ATxID: string; AVerbose: boolean = false): string;
begin
  result := Post(
       Format('{"jsonrpc": "1.0", "id":"DMSExposed", "method": "getrawtransaction", "params": ["%s",%s] }',
              [ATxID, BoolToStr(AVerbose,true).ToLower]));
end;

function TDocumentchain.StoreHex(const AHex: string; const AFee: real; const AMinConfirms: integer): string;
var i,
    LVout: integer;
    LChange: boolean;
    LMinMax,
    LTmpAmount,
    LInputAmount: real;
    s,
    LChangeAdr,
    LChangeStr,
    LTrans,
    LTxid: string;
    LJson,
    LJson2,
    LTx  : TJsonObject;
    LJSAr: TJsonArray;
begin
  s := Post('{"jsonrpc":"1.0","id":"DMSExposed","method":"listunspent"}');
  LTxid := '';
  LVout := -1;
  LInputAmount := -1;
  result := '';
  LJson := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(s),0) as TJSONObject;
  try
    LJSAr := TJSONArray(LJson.Get('result').JsonValue);
    // search input
    LMinMax := 21000000;
    for i:=0 to LJSAr.Count-1 do begin
      LTx := TJSONObject(LJSAr.Get(i));
      LTmpAmount := LTx.GetValue<real>('amount');
      if  (LTx.GetValue<boolean>('spendable') = true)
      and (LTx.GetValue<integer>('confirmations') >= AMinConfirms)
      and (LTmpAmount >= AFee)
      and (LTmpAmount <= cMaxInputAmount)  // vorerst Sicherheitswert für max. Guthaben
      and (LTmpAmount <  LMinMax)
      then begin
        LTxid := LTx.GetValue<string>('txid');
        LVout := LTx.GetValue<integer>('vout');
        LInputAmount := LTmpAmount;
        LMinMax := LInputAmount;
      end;
    end;
  finally
    LJson.Free;
  end;
  if LTxid.IsEmpty then
    raise Exception.CreateHelp('No matching input found.', 2719);

  // change amount
  LChange := (LInputAmount - AFee) > (AFee / 100);
  if LChange then begin
    LChangeAdr := Post('{"jsonrpc":"1.0","id":"DMSExposed","method":"getrawchangeaddress"}');
    LChangeAdr := GetResultFromJSON(LChangeAdr);
    LChangeStr := Format('"%s":%s, ', [LChangeAdr, FloatToJson(LInputAmount - AFee)]);
  end
  else
    LChangeStr := '';

  // CreateRawTransaction...
  LTrans := Post('{"jsonrpc":"1.0","id":"DMSExposed","method":"createrawtransaction","params":'
                + Format('[ [{"txid":"%s","vout":%d}], {%s"data":"%s"} ] }',
                         [LTxid, LVout, LChangeStr, AHex]));
  // {"result":"02000000019d0150631f8ce5cb8ac8781baf1ffb7994e50cad50906717cccdd82325937ef60000000000ffffffff0100000000000000000c6a0a48616c6c6f2057656c7400000000","error":null,"id":"DMSExposed"}
  LTrans := GetResultFromJSON(LTrans);

  (* Debug
  s := Post(Format('{"jsonrpc":"1.0","id":"DMSExposed","method":"decoderawtransaction","params":["%s"]}', [LTrans]));
  ShowMessage(s);
  *)

  // sign
  s := Post(Format('{"jsonrpc":"1.0","id":"DMSExposed","method":"signrawtransaction","params":["%s"]}', [LTrans]));
  // {"result":{"hex":"02000000019d0 .. 000000","complete":true},"error":null,"id":"DMSExposed"}
  LJson := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(s),0) as TJSONObject;
  try
    LJson2 := LJson.Values['result'] as TJSONObject;
    if LJson2.GetValue<boolean>('complete') = false then
      raise Exception.Create('Signierung konnte nicht abgeschlossen werden.');
    LTrans := LJson2.GetValue<string>('hex');
  finally
    LJson.Free;
  end;

  // send...
  LTrans := Post(Format('{"jsonrpc":"1.0","id":"DMSExposed","method":"sendrawtransaction","params":["%s"]}', [LTrans]));
  LTrans := GetResultFromJSON(LTrans);
  result := LTrans; // the txid
end;

function TDocumentchain.StoreText(const AText: string;
                                  const AFee: real = cFeedStoreDocInfo; const AMinConfirms: integer = 6): string;
begin
  result := StoreHex(String2Hex(AText), AFee, AMinConfirms);
end;

function TDocumentchain.StoreDocumentInfo(const ADocInfo: TBlockchainDocument;
                                          const AFee: real = cFeedStoreDocInfo; const AMinConfirms: integer = 6): string;
begin
  try
    result := StoreHex(ADocInfo.FDocDataHex, AFee, AMinConfirms);
  except  // Wallet DMS Core running? Start it
    raise;
  end;
end;

function TDocumentchain.FGetDocument(const ATxID: string): TBlockchainDocument;
var i: integer;
    s,
    LRawTx: string;
    LJson,
    LJson2: TJsonObject;
    LJSAr : TJsonArray;
begin
  result.Loaded := false;
  LRawTx := GetRawTransaction(ATxID, true);
  LJson := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(LRawTx),0) as TJSONObject;
  try
    LJson2 := LJSon.GetValue('result') as TJsonObject;
    // Blockdaten...
    try
      result.FConfirms := LJson2.GetValue<integer>('confirmations');
    except
      raise Exception.Create('Wrong transaction ID or block has not yet been mined.');
    end;
    result.FTimeUTC := UnixToDateTime(LJson2.GetValue<int64>('time'));
    result.FBlockTimeUTC := UnixToDateTime(LJson2.GetValue<int64>('blocktime'));
    result.FHeight := LJson2.GetValue<integer>('height');
    result.FBlockHash := LJson2.GetValue<string>('blockhash');
    // vout mit den Document-Daten suchen...
    LJSAr := TJSONArray(LJson2.Get('vout').JsonValue);
    for i:=0 to LJSAr.Count-1 do begin
      LJson2 := TJsonObject(LJSAr.Get(i));   // .Items[i]
      // {"value": 0.00000000,"valueSat": 0,"n": 1,"scriptPubKey": {"asm": "OP_RETURN 48616c6c6f2057656c74","hex": "6a0a48616c6c6f2057656c74","type": "nulldata"}}
      if LJson2.Values['scriptPubKey'] = nil then
        Continue;
      LJson2 := LJSon2.GetValue('scriptPubKey') as TJsonObject;
      // {"asm": "OP_RETURN 48616c6c6f2057656c74","hex": "6a0a48616c6c6f2057656c74","type": "nulldata"}
      if (LJson2.GetValue<string>('type') = 'nulldata') then begin
        s := LJson2.GetValue<string>('asm');
        if s.StartsWith('OP_RETURN ') then begin
          s := s.Remove(0, 10);
          result.FillData(s);
          result.Loaded := true;
          break;
        end;
      end;
    end;
  finally
    LJson.Free;
  end;
end;

function TDocumentchain.GetDocument(const ATxID: string): TBlockchainDocument;
begin
  try
    result := FGetDocument(ATxID);
  except  // Wallet DMS Core running? Start it
    raise;
  end;
end;

end.

