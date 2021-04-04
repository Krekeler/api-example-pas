{ Documentchain (DMS Core) Example Code https://documentchain.org}
{ Please do not hesitate to contact us if you have any questions }
{ mail@documentchain.org, https://documentchain.org/legal-notice/}

unit DMS.Api;

interface

uses
  System.Classes, System.json, System.Net.HttpClient, System.Net.URLClient;

const
  cFeeStoreDocInfo = 0.1;   // blockchain transaction fee to store document hashes
  cWebApiUrl = 'https://api.dms.cash/';

type
  TWalletConfiguration = record   // local Wallet "DMS Core" configuration dms.conf
  private
    procedure LoadConf;
  public
    Testnet: boolean;
    rpcuser,
    rpcpassword,
    rpcallowip: string;
    function DataDir: string;
    function ConfFileName: string;
    class function CreateNew(ATestnet: boolean = false): TWalletConfiguration; static;
  end;

  TAuditStatus = (auditUnknown,
                  auditOk,
                  auditHint,
                  auditViolation);

  TBlockchainDocument = record
  private
    // Blockchain
    FTimeUTC,
    FBlockTimeUTC: TDateTime;
    FHeight,
    FConfirms: integer;
    FBlockHash: string;
    // document
    FGUID,
    FIndexHashMD5,
    FFileHashSHA512, // a secure file hash is required, the other hashes are optional
    FAttrHashSHA256,
    FOwnerHashSHA256: string;
    FDocDataHex  : string;
  public
    procedure FillData(const AGUID, AIndexHashMD5,
                       AFileHashSHA512, AAttrHashSHA256, AOwnerHashSHA256: string); overload;
    procedure FillData(const AHex: string); overload;
    property GUID: string read FGUID;
    property IndexHashMD5: string read FIndexHashMD5;
    property BlockTimeUTC: TDateTime read FBlockTimeUTC;
    class function CompareDocuments(const ACurrDoc, AStoredDoc: TBlockchainDocument;
                                    AResults: TStrings = nil): TAuditStatus; static;
  end;

  // local "DMS Core" or WebAPI
  TCustomWallet = class(TObject)
  private
    FWinHTTP: THTTPClient;
    FURL: string;  // 'http://127.0.0.1:8332' or 'https://api.dms.cash/'
    FCmdGetinfo: string;
    FCmdGetRawTx: string;
    function GetValueFromJSON(const AJson, AValueName: string): string;
    function GetResultFromJSON(const AJson: string): string;
    function Post(const ACommand: string): string; virtual; abstract;
    function GetRawTransaction(const ATxID: string): string;
    function StoreDocumentInfo(const ADocInfo: TBlockchainDocument): string; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetInfo(ASL: TStrings);
    property URL: string read FURL;
  end;

  TWalletDMSCore = class(TCustomWallet)
  private
    FRPCUser,
    FRPCPassword: string;
    const
      cCmdGetinfo  = '{"jsonrpc": "1.0", "id":"DMSExposed", "method": "getinfo", "params": [] }';
      cCmdGetRawTx = '{"jsonrpc": "1.0", "id":"DMSExposed", "method": "getrawtransaction", "params": ["%s",true] }';
    function Post(const ACommand: string): string; override;
    function StoreHex(const AHex: string; const AFee: real; const AMinConfirms: integer): string;
    function StoreDocumentInfo(const ADocInfo: TBlockchainDocument): string; override;
    procedure HTTPClientAuthEvent(const Sender: TObject; AnAuthTarget: TAuthTargetType; const ARealm,
      AURL: string; var AUserName, APassword: string; var AbortAuth: Boolean; var Persistence: TAuthPersistenceType);
  public
    constructor Create(ATestnet: boolean = false);
  end;

  TWalletWebAPI = class(TCustomWallet)
  private
    FTestnet: boolean;
    FAccount: string; // account info to pay the fee, you need this in mainnet, ask mail@documentchain.org
    const
      cCmdGetinfo  = 'api=getinfo' + sLineBreak + 'id=DMSExposed';
      cCmdGetRawTx = 'api=getrawtransaction' + sLineBreak + 'id=DMSExposed' + sLineBreak
                   + 'tx=%s' + sLineBreak + 'verbose=1';
    function Post(const ACommand: string): string; override;
    function StoreDocumentInfo(const ADocInfo: TBlockchainDocument): string; override;
  public
    constructor Create(const AURL, AAccount: string; ATestnet: boolean);
  end;

  TDocumentchain = class(TComponent)
  strict private
    FWallet: TCustomWallet;
    function FGetDocument(const ATxID: string): TBlockchainDocument;
  private
  public
    constructor Create(Owner: TComponent; AUseWebAPI, ATestnet: boolean;
                       const AAccount: string);
    destructor Destroy; override;
    function IsWebAPI: boolean;
    function StoreDocumentInfo(const ADocInfo: TBlockchainDocument): string;
    function GetDocument(const ATxID: string): TBlockchainDocument;
    class function ValidTransactionID(const ATxID: string): boolean;
  end;

implementation

uses
  Winapi.Windows{HKEY}, System.SysUtils, System.DateUtils, System.IOUtils, System.StrUtils,
  System.Hash, System.Win.Registry, System.ZLib, System.UITypes, System.Generics.Collections,
  FMX.DialogService.Async;

// https://github.com/Krekeler/documentchain/blob/master/dms-docs/document-revision-data.md
const cDocMagicChars     = 'DM$';
      cDocMagicCharsHex  = '444D24';
      cDocDataVersion    = 2;
      cDocDataVersionHex = '0002';
      cDocTypeAppHex     = '0099'; // app id
      cPrefixGUID        = '00';
      cPrefixFile        = 'F0';
      cPrefixAttr        = 'A0';
      cPrefixOwner       = 'B0';
      cPrefixMD5         = '00';
      cPrefixSha256      = '22';
      cPrefixSha512      = '25';
      cEmptyMD5Hash      = 'D41D8CD98F00B204E9800998ECF8427E';
      cEmptySHA256Hash   = 'E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855';
      cMaxInputAmount    = 55; // do not take any inputs with a higher credit for safety reasons
      cRPCCommandFmt     = '{ "jsonrpc": "1.0", "id":"DMSExposed", "method": "%s", "params": [%s] }';

function String2Hex(const AStr: string): string;
var r: RawByteString;
begin
  r := UTF8Encode(AStr);
  SetLength(result, 2 * Length(r));
  BinToHex(@r[1], PWideChar(result), Length(r));
end;

function Hex2String(const AHex: string): string;
var r: RawByteString;
begin
  SetLength(r, Length(AHex) div 2);
  HexToBin(PWideChar(AHex), r[1], Length(AHex));
  result := UTF8ToString(r);
end;

function HashHexLength(const AAlgo: integer): integer;
begin
  case AAlgo of
     0:  result := 32;  // GUID/MD5
     1:  result := 40;  // SHA1
    21:  result := 56;  // SHA2-224
    22:  result := 64;  // SHA2-256
    23:  result := 96;  // SHA2-384
    25:  result := 128; // SHA2-512
    31:  result := 56;  // SHA3-224
    32:  result := 64;  // SHA3-256
    33:  result := 96;  // SHA3-384
    35:  result := 128; // SHA3-512
    else result := 0;
  end;
end;

{ TWalletConfiguration }

class function TWalletConfiguration.CreateNew(ATestnet: boolean = false): TWalletConfiguration;
begin
  result.Testnet := ATestnet;
  result.rpcuser := '';
  result.rpcpassword := '';
  result.rpcallowip := '127.0.0.1';
  result.LoadConf;
end;

function TWalletConfiguration.DataDir: string;
begin
  with TRegistry.Create(KEY_READ) do
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly('Software\Krekeler\DMS-Qt') and ValueExists('strDataDir') then
      result := ReadString('strDataDir');
    Free;
  except
    Free;
    result := '';
  end;
end;

function TWalletConfiguration.ConfFileName: string;
begin
  result := DataDir;
  if not result.IsEmpty then
    result := TPath.Combine(result, 'dms.conf');
end;

procedure TWalletConfiguration.LoadConf;
{read relevant data from dms.conf}
var LFileName: string;
begin
  // read dms.conf
  LFileName := ConfFileName;
  if (not LFileName.IsEmpty) and TFile.Exists(LFileName) then
    with TStringList.Create do
    try
      LoadFromFile(LFileName);
      rpcuser     := Values['rpcuser'];
      rpcpassword := Values['rpcpassword'];
      rpcallowip  := Values['rpcallowip'];
    finally
      Free;
    end;
end;

{ TCustomWallet }

constructor TCustomWallet.Create;
begin
  inherited;
  FWinHTTP := THTTPClient.Create;
end;

destructor TCustomWallet.Destroy;
begin
  FWinHTTP.Free;
  inherited;
end;

procedure TCustomWallet.GetInfo(ASL: TStrings);
var LRes : string;
    LJson: TJsonObject;
    LObj : TJsonObject;
    LPair: TJsonPair;
    LVal : TJsonValue;
begin
  LRes := Post(FCmdGetinfo);
  LJson := TJsonObject.Create;
  try
    if LJson.Parse(BytesOf(LRes), 0) > 0 then begin
      LObj := LJson.GetValue('result') as TJsonObject;
      if Assigned(LObj) then
        for LPair in LObj do
          ASL.Values[LPair.JsonString.ToString.Trim(['"'])] := LPair.JsonValue.ToString;
      if Self is TWalletWebAPI then begin
        LVal := LJson.GetValue('ver');
        if Assigned(LVal) then
          ASL.Values['webapiversion'] := LVal.ToString;
        LVal := LJson.GetValue('url');
        if Assigned(LVal) then
          ASL.Values['url'] := LVal.ToString.Replace('\/', '/', [rfReplaceAll]);
      end;
    end;
  finally
    LJson.free;
  end;
end;

function TCustomWallet.GetRawTransaction(const ATxID: string): string;
begin
  result := Post(Format(FCmdGetRawTx, [ATxID]));
end;

function TCustomWallet.GetValueFromJSON(const AJson, AValueName: string): string;
begin
  with TJsonobject.Create do
  try
    if Parse(BytesOf(AJson), 0) > 0 then
      result := GetValue<string>(AValueName);
  finally
    Free;
  end;
end;

function TCustomWallet.GetResultFromJSON(const AJson: string): string;
begin
  result := GetValueFromJSON(AJson, 'result');
end;

{ TWalletDMSCore }

constructor TWalletDMSCore.Create(ATestnet: boolean = false);
var LConf: TWalletConfiguration;
begin
  inherited Create;
  // load configuration from dms.conf
  LConf := TWalletConfiguration.CreateNew(ATestnet);
  FRPCUser := LConf.rpcuser;
  FRPCPassword := LConf.rpcpassword;
  FURL := 'http://127.0.0.1:' + IfThen(ATestnet, '41420', '41320');
  { TODO : set RPC and URL if wallet "DMS Core" is not located on local host }
  FWinHTTP.AuthEvent := HTTPClientAuthEvent;
  FCmdGetinfo := cCmdGetinfo;
  FCmdGetRawTx := cCmdGetRawTx;
end;

procedure TWalletDMSCore.HTTPClientAuthEvent(const Sender: TObject; AnAuthTarget: TAuthTargetType; const ARealm,
  AURL: string; var AUserName, APassword: string; var AbortAuth: Boolean; var Persistence: TAuthPersistenceType);
begin
  AUserName := FRPCUser;
  APassword := FRPCPassword;
end;

function TWalletDMSCore.Post(const ACommand: string): string;
var LJsonToSend: TStringStream;
begin
  LJsonToSend := TStringStream.Create(ACommand);
  try
    result := FWinHTTP.Post(FURL, LJsonToSend).ContentAsString;
  finally
    LJsonToSend.Free;
  end;
end;

function TWalletDMSCore.StoreHex(const AHex: string; const AFee: real; const AMinConfirms: integer): string;
var i,
    LTmpConfirms,
    LVout: integer;
    LChange: boolean;
    LMinMax,
    LTmpAmount,
    LInputAmount: double;
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
  // 1. get input to pay the fee
  s := Post('{"jsonrpc":"1.0","id":"DMSExposed","method":"listunspent"}');
  LTxid := '';
  LVout := -1;
  LInputAmount := -1;
  result := '';
  LJson := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(s),0) as TJSONObject;
  if not Assigned(LJson) then // s = ''
    raise Exception.Create('No data received from wallet.');
  try
    LJSAr := TJSONArray(LJson.Get('result').JsonValue);
    LMinMax := 21000000;
    for i:=0 to LJSAr.Count-1 do begin
      LTx := TJSONObject(LJSAr.Items[i]);
      LTmpConfirms := LTx.GetValue<integer>('confirmations');
      LTmpAmount := LTx.GetValue<real>('amount');
      if  (LTx.GetValue<boolean>('spendable') = true)
      and (LTmpConfirms >= AMinConfirms)
      and (LTmpAmount >= AFee)
      and (LTmpAmount <= cMaxInputAmount)
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
    raise Exception.Create('No matching input found.');

  // 2. change
  LChange := (LInputAmount - AFee) > (AFee / 100);
  if LChange then begin
    LChangeAdr := Post('{"jsonrpc":"1.0","id":"DMSExposed","method":"getrawchangeaddress"}');
    LChangeAdr := GetResultFromJSON(LChangeAdr);
    LChangeStr := FloatToStrF(LInputAmount - AFee, ffFixed, 15, 8, TFormatSettings.Invariant);
    LChangeStr := Format('"%s":%s, ', [LChangeAdr, LChangeStr]);
  end
  else
    LChangeStr := '';

  // 3. create raw transaction
  LTrans := Post('{"jsonrpc":"1.0","id":"DMSExposed","method":"createrawtransaction","params":'
                + Format('[ [{"txid":"%s","vout":%d}], {%s"data":"%s"} ] }',
                         [LTxid, LVout, LChangeStr, AHex]));
  // {"result":"02000000019d0150631f8ce5cb8ac8781baf1ffb7994e50cad50906717cccdd82325937ef60000000000ffffffff0100000000000000000c6a0a48616c6c6f2057656c7400000000","error":null,"id":"DMSExposed"}
  LTrans := GetResultFromJSON(LTrans);

  // 4. sign
  s := Post(Format('{"jsonrpc":"1.0","id":"DMSExposed","method":"signrawtransaction","params":["%s"]}', [LTrans]));
  // {"result":{"hex":"02000000019d0 .. 000000","complete":true},"error":null,"id":"DMSExposed"}
  LJson := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(s),0) as TJSONObject;
  try
    LJson2 := LJson.Values['result'] as TJSONObject;
    if LJson2.GetValue<boolean>('complete') = false then
      raise Exception.Create('Signing could not be completed.');
    LTrans := LJson2.GetValue<string>('hex');
  finally
    LJson.Free;
  end;

  // 5. send transaction
  LTrans := Post(Format('{"jsonrpc":"1.0","id":"DMSExposed","method":"sendrawtransaction","params":["%s"]}', [LTrans]));
  LTrans := GetResultFromJSON(LTrans);
  result := LTrans; // tx id
end;

function TWalletDMSCore.StoreDocumentInfo(const ADocInfo: TBlockchainDocument): string;
begin
  try
    result := StoreHex(ADocInfo.FDocDataHex, cFeeStoreDocInfo, 6);
  except
    on E:Exception do begin
      if (E.Message.Contains('10061') or E.Message.Contains('500 Internal Server Error'))
      then
        TDialogServiceAsync.MessageDialog('Please start DMS Core and unlock it if necessary.',
                                          TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0)
      else
        raise;
    end;
  end;
end;

{ TWalletWebAPI }

constructor TWalletWebAPI.Create(const AURL, AAccount: string; ATestnet: boolean);
begin
  inherited Create;
  FURL := AURL;
  FAccount := AAccount;
  FTestnet := ATestnet;
  FCmdGetinfo := cCmdGetinfo;
  FCmdGetRawTx := cCmdGetRawTx;
end;

function TWalletWebAPI.Post(const ACommand: string): string;
var LParams: TStrings;
begin
  LParams := TStringList.Create;
  try
    LParams.Text := ACommand;
    LParams.Values['account'] := FAccount;
    if FTestnet then
      LParams.Values['testnet'] := '1';
    result := FWinHTTP.Post(FURL, LParams).ContentAsString;
  finally
    LParams.Free;
  end;
end;

function TWalletWebAPI.StoreDocumentInfo(const ADocInfo: TBlockchainDocument): string;
var LParams: TStrings;
    LResponse,
    LErr: string;
begin
  LParams := TStringList.Create;
  try
    LParams.Values['api'] := 'post';
    LParams.Values['raw'] := ADocInfo.FDocDataHex;
    LParams.Values['account'] := FAccount;
    if FTestnet then
      LParams.Values['testnet'] := '1';
    LResponse := FWinHTTP.Post(FURL, LParams).ContentAsString;
  finally
    LParams.Free;
  end;
  try
    result := GetValueFromJSON(LResponse, 'txid'); // exception if not included
    if result.IsEmpty then                         // exception if empty
      raise Exception.Create('Transaction ID is empty.');
  except
    on E:Exception do begin
      // show error message from API server, like "account required to post a document in mainnet"
      LErr := GetValueFromJSON(LResponse, 'err');
      raise Exception.Create('Document hashes could not be stored.'
                           + sLineBreak + E.Message
                           + IfThen(not LErr.IsEmpty, sLineBreak+sLineBreak + 'Server message:' + sLineBreak + LErr));
    end;
  end;
end;

{ TBlockchainDocument }

function CleanDataToken(const ASrc: string): string;
{trim and store only LF line breaks}
begin
  result := ASrc.Trim;
  result := result.Replace(#13#10, #10, [rfReplaceAll]);
  result := result.Replace(#13, #10, [rfReplaceAll]);
end;

procedure TBlockchainDocument.FillData(const AGUID, AIndexHashMD5,
                                       AFileHashSHA512, AAttrHashSHA256, AOwnerHashSHA256: string);
begin
  // a secure file hash is required, guid and the other hashes are optional
  FGUID := AGUID;
  FIndexHashMD5    := IfThen(not SameText(AIndexHashMD5,   cEmptyMD5Hash), AIndexHashMD5).ToUpper;
  FFileHashSHA512  := IfThen(not SameText(AFileHashSHA512, THashSHA2.GetHashString('', SHA512)), AFileHashSHA512).ToUpper;
  FAttrHashSHA256  := IfThen(not SameText(AAttrHashSHA256, cEmptySHA256Hash), AAttrHashSHA256).ToUpper;
  FOwnerHashSHA256 := IfThen(not SameText(AOwnerHashSHA256,cEmptySHA256Hash), AOwnerHashSHA256).ToUpper;
  // serialize
  FDocDataHex := cDocMagicCharsHex
               + cDocDataVersionHex
               + cDocTypeAppHex
               + IfThen(not FGUID.IsEmpty,
                   cPrefixGUID + cPrefixMD5 + FGUID.Replace('{', '').Replace('-', '', [rfReplaceAll]).Replace('}', ''))
               + IfThen(not FIndexHashMD5.IsEmpty,   // short index hash
                   cPrefixFile + cPrefixMD5 + FIndexHashMD5)
               + IfThen(not FFileHashSHA512.IsEmpty, // secure file hash
                   cPrefixFile + cPrefixSha512 + FFileHashSHA512)
               + IfThen(not FAttrHashSHA256.IsEmpty,
                   cPrefixAttr + cPrefixSHA256 + FAttrHashSHA256)
               + IfThen(not FOwnerHashSHA256.IsEmpty,
                   cPrefixOwner + cPrefixSHA256 + FOwnerHashSHA256);
end;

procedure TBlockchainDocument.FillData(const AHex: string); // Value is Hex/Base64 from Blockchain
{https://github.com/Krekeler/documentchain/blob/master/dms-docs/document-revision-data.md}
begin
  FDocDataHex := AHex.ToUpper;
  FGUID := '';
  FIndexHashMD5 := '';
  FFileHashSHA512 := '';
  FAttrHashSHA256 := '';
  FOwnerHashSHA256 := '';
  // magic chars
  if not SameText(FDocDataHex.Substring(0, 6), cDocMagicCharsHex) then
    raise Exception.Create('Data cannot be converted, wrong format.');
  // Blockchain Data Version
  // "0001"= Revision v1, not considered in this example
  // "0002"= Revision v2
  if FDocDataHex.Substring(6, 4) <> '0002' then
    raise Exception.Create('Unknown data version.');
  // deserialize
  var idx: integer := 14;
  var i  : integer := 0;
  var htype: char;
  var halgo: string;
  var hash : string;
  while (idx < FDocDataHex.Length) do begin
    inc(i); // wrong data could cause an endless loop
    if (i > 19) then
      raise Exception.Create('Invalid hash data');
    htype := FDocDataHex.Chars[idx];
    halgo := FDocDataHex.Substring(idx+2, 2);
    hash  := FDocDataHex.Substring(idx+4, HashHexLength(halgo.ToInteger));
    idx   := idx + 4 + HashHexLength(halgo.ToInteger);
    case htype of
      '0': FGUID := hash;
      'F': if halgo = cPrefixMD5 then
             FIndexHashMD5 := hash
           else if halgo = cPrefixSha512 then
             FFileHashSHA512 := hash;
      'A': FAttrHashSHA256 := hash;
      'B': FOwnerHashSHA256 := hash;
    end;
  end;
  // GUID...
  FGUID := '{' + FGUID + '}';
  FGUID := FGUID.Insert(9,  '-');
  FGUID := FGUID.Insert(14, '-');
  FGUID := FGUID.Insert(19, '-');
  FGUID := FGUID.Insert(24, '-');
  StringToGUID(FGUID); // assert
end;

class function TBlockchainDocument.CompareDocuments(const ACurrDoc, AStoredDoc: TBlockchainDocument;
                                   AResults: TStrings = nil): TAuditStatus;
var LBlockTime: string;
begin
  if  (ACurrDoc.FGUID = AStoredDoc.FGUID)
  and (ACurrDoc.FFileHashSHA512 = AStoredDoc.FFileHashSHA512)
  and (ACurrDoc.FAttrHashSHA256 = AStoredDoc.FAttrHashSHA256)
  and (AStoredDoc.FOwnerHashSHA256 = AStoredDoc.FOwnerHashSHA256)
  then
    result := auditOk
  else begin
    if (ACurrDoc.FGUID <> AStoredDoc.FGUID)
    or (ACurrDoc.FFileHashSHA512 <> AStoredDoc.FFileHashSHA512)
    then
      result := auditViolation
    else
      result := auditHint;
  end;

  if Assigned(AResults) then begin
    LBlockTime := DateTimeToStr(AStoredDoc.FBlockTimeUTC) + ' UTC';
    case result of
      auditOk       : AResults.Add(Format('OK. The document has not been changed since %s', [LBlockTime]));
      { TODO : In the final software, a certificate for the customer can be created here. }
      auditHint     : AResults.Add(Format('File OK, Attribute changed. The document file has not been changed since %s',
                                          [LBlockTime]));
      auditViolation: AResults.Add('No confirmation! The document has been modified or it is another document.');
    end;
    AResults.Add('');
    AResults.Add('Block Timestamp: ' + LBlockTime);
  end;
end;

{ TDocumentchain }

constructor TDocumentchain.Create(Owner: TComponent; AUseWebAPI, ATestnet: boolean;
                                  const AAccount: string);
begin
  inherited Create(Owner);
  if AUseWebAPI then
    FWallet := TWalletWebAPI.Create(cWebApiUrl, AAccount, ATestnet)
  else
    FWallet := TWalletDMSCore.Create(ATestnet);
end;

destructor TDocumentchain.Destroy;
begin
  FWallet.Free;
  inherited;
end;

function TDocumentchain.FGetDocument(const ATxID: string): TBlockchainDocument;
{read document from blockchain}
const cErrorTx = 'Document data could not be loaded from blockchain. Wrong transaction ID '
               + 'or the block has not been confirmed (mined) yet.';
var i: integer;
    s,
    LRawTx: string;
    LJson,
    LJson2: TJsonObject;
    LJSAr : TJsonArray;
begin
  LRawTx := FWallet.GetRawTransaction(ATxID);
  if LRawTx.IsEmpty then
    raise Exception.Create(cErrorTx);
  LJson := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(LRawTx),0) as TJSONObject;
  try
    try
      LJson2 := LJson.GetValue('result') as TJsonObject;
      result.FConfirms := LJson2.GetValue<integer>('confirmations');
    except
      raise Exception.Create(cErrorTx);
    end;
    result.FTimeUTC := UnixToDateTime(LJson2.GetValue<int64>('time'));
    result.FBlockTimeUTC := UnixToDateTime(LJson2.GetValue<int64>('blocktime'));
    result.FHeight := LJson2.GetValue<integer>('height');
    result.FBlockHash := LJson2.GetValue<string>('blockhash');
    // search vout
    LJSAr := TJSONArray(LJson2.Get('vout').JsonValue);
    for i:=0 to LJSAr.Count-1 do begin
      LJson2 := TJsonObject(LJSAr.Items[i]);
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
  except
    on E:Exception do begin
      if  (FWallet is TWalletDMSCore)
      and (E.Message.Contains('10061') or E.Message.Contains('500 Internal Server Error'))
      then
        TDialogServiceAsync.MessageDialog('Please start DMS Core and unlock it if necessary.',
                                          TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0)
      else
        raise;
    end;
  end;
end;

function TDocumentchain.IsWebAPI: boolean;
begin
  result := FWallet is TWalletWebAPI;
end;

function TDocumentchain.StoreDocumentInfo(const ADocInfo: TBlockchainDocument): string;
begin
  result := FWallet.StoreDocumentInfo(ADocInfo);
end;

class function TDocumentchain.ValidTransactionID(const ATxID: string): boolean;
{256 bit, 32 byte, 64 chars as Hex}
begin
  result := ATxID.Length = 64;
end;

end.

