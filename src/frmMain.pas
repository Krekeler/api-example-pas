{ Documentchain (DMS Core) Example Code https://documentchain.org}

unit frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Edit;

type
  TFormMain = class(TForm)
    Image1: TImage;
    gbChain: TGroupBox;
    rbTestnet: TRadioButton;
    rbMainnet: TRadioButton;
    edFileName: TEdit;
    btnStore: TButton;
    edTxid: TEdit;
    btnRevision: TButton;
    lblExample: TLabel;
    lblFileHash: TLabel;
    OpenDlg: TOpenDialog;
    lblPleaseWait: TLabel;
    procedure btnStoreClick(Sender: TObject);
    procedure btnRevisionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    RpcUser,
    RpcPW: string;
    function GetRpcUrl: string;
    function GetAttributeHash: string;
    function GetFileHash: string;
  public
  end;

var
  FormMain: TFormMain;

implementation

uses System.IOUtils,
   //Winapi.ActiveX, // CoCreateGUID
     IdGlobal, IdHash, IdHashMessageDigest, // Indy hash functions
     DMS.Api;

{$R *.fmx}

function GetGUID: string;
var G: TGUID;
begin
  { TODO : you can use the document GUID from your Document Management }

  { to create a GUID on Windows (uses Winapi.ActiveX):
  CoCreateGUID(G);
  result := GuidToString(G);
  }
  { create GUID in Delphi IDE: press Shift+Ctrl+G }

  // in this API example we use a dummy GUID:
  result := '{00000000-0000-0000-0000-000000000000}';
end;

function TFormMain.GetRpcUrl: string;
begin
  if rbMainnet.IsChecked then
    result := 'http://127.0.0.1:41320'
  else if rbTestnet.IsChecked then
    result := 'http://127.0.0.1:41420'
  else
    raise Exception.Create('Select Testnet or Mainnet first');
end;

function TFormMain.GetAttributeHash: string;
var s: string;
begin
  { TODO : you can hash doc attributes from your Document Management, like: }
  {
  s := Dataset.FieldByName('name').AsString
     + Dataset.FieldByName('number').AsString
     + Dataset.FieldByName('guid').AsString
     + Dataset.FieldByName('date').AsString;
  }
  // here we hash the file path
  s := edFileName.Text;
  if s.IsEmpty then
    result := '' // not the hash of ''
  else
    with TIdHashMessageDigest5.Create do
    try
      result := HashStringAsHex(s);
    finally
      Free;
    end;
end;

function TFormMain.GetFileHash: string;
var LHasher: TIdHashMessageDigest5;
    LStream: TFileStream;
begin
  LHasher := TIdHashMessageDigest5.Create;
  LStream := TFileStream.Create(edFileName.Text, fmOpenRead or fmShareDenyWrite);
  try
    result := LHasher.HashStreamAsHex(LStream);
  finally
    LStream.Free;
    LHasher.Free;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  { TODO : enter RPC User and password from dms.conf }
  RpcUser := '';     // dms.conf: rpcuser
  RpcPW := '';       // dms.conf: rpcpassword
end;

procedure TFormMain.btnStoreClick(Sender: TObject);
var LChain: TDocumentchain;
    LDoc  : TBlockchainDocument;
    LHash,
    LTxid : string;
begin
  if not TFile.Exists(edFileName.Text) then begin
    if OpenDlg.Execute then
      edFileName.Text := OpenDlg.FileName
    else
      exit;
  end;
  if not TFile.Exists(edFileName.Text) then
    raise Exception.Create('File not found');
  { dms.conf example:
  rpcuser=dmsrpcuser
  rpcpassword=gdbc6asdkHhd6F
  rpcallowip=127.0.0.1
  server=1
  listen=1
  daemon=1
  }
  LHash := GetFileHash;
  lblFileHash.Text := 'Hash: ' + LHash;
  lblFileHash.Visible := true;

  LChain := TDocumentchain.Create(Self,
                                  GetRpcUrl,
                                  RpcUser,
                                  RpcPW);
  try
    LDoc.FillData(GetGUID,
                  LHash,
                  GetAttributeHash,
                  ''); // optional hash from publisher name
    LTxID := LChain.StoreDocumentInfo(LDoc);
    lblPleaseWait.Visible := true;
    { TODO : store transaction id with the document, i.e. in archive database }
    edTxid.Text := LTxID; // like "4a3f1e8df231296f61ce081b84ed9f54b896e95a31090690f44596433602e2fe"
  finally
    LChain.Free;
  end;
end;

procedure TFormMain.btnRevisionClick(Sender: TObject);
var LChain: TDocumentchain;
    LDoc,                         // current local document
    LDocBC: TBlockchainDocument;  // document information on blockchain
    LHash,
    LTxid : string;
    LLog  : TStrings;
begin
  if not TFile.Exists(edFileName.Text) then
    raise Exception.Create('File not found');
  LHash := GetFileHash;
  lblFileHash.Text := 'Hash: ' + LHash;
  lblFileHash.Visible := true;
  LLog := TStringList.Create;
  // current document
  LChain := TDocumentchain.Create(Self,
                                  GetRpcUrl,
                                  RpcUser,
                                  RpcPW);
  try
    LDoc.FillData(GetGUID,
                  GetFileHash,
                  GetAttributeHash,
                  ''); // optional hash from publisher name
    LTxID := edTxid.Text; { TODO : use the stored transaction id }
    LDocBC := LChain.GetDocument(LTxID);
    // compare the documents
    TBlockchainDocument.CompareDocuments(LDoc, LDocBC, LLog, false);
    ShowMessage(LLog.Text);
  finally
    LLog.Free;
    LChain.Free;
  end;
end;

end.
