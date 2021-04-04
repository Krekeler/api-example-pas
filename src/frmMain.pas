{ Documentchain (DMS Core) Example Code https://documentchain.org}
{ Please do not hesitate to contact us if you have any questions }
{ mail@documentchain.org, https://documentchain.org/legal-notice/}

unit frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Objects, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Edit, FMX.Dialogs;

type
  TFormMain = class(TForm)
    Image1: TImage;
    gbChain: TGroupBox;
    rbTestnetWeb: TRadioButton;
    rbTestnetLocal: TRadioButton;
    edFileName: TEdit;
    btnStore: TButton;
    edTxid: TEdit;
    btnRevision: TButton;
    lblExample: TLabel;
    lblFileHash: TLabel;
    OpenDlg: TOpenDialog;
    lblPleaseWait: TLabel;
    rbMainnetWeb: TRadioButton;
    rbMainnetLocal: TRadioButton;
    procedure btnStoreClick(Sender: TObject);
    procedure btnRevisionClick(Sender: TObject);
  private
    function GetAttributeHash: string;
    function CheckConfiguration: boolean;
  public
  end;

var
  FormMain: TFormMain;

implementation

uses System.IOUtils, System.Hash, FMX.DialogService.Async, FMX.DialogService.Sync,
     DMS.Api;

{$R *.fmx}

function GetGUID: string;
begin
  { TODO : you can use the document GUID from your Document Management }
  { create GUID: TGUID.New                        }
  { create GUID in Delphi IDE: press Shift+Ctrl+G }
  // in this API example we use a dummy GUID:
  result := '{00000000-0000-0000-0000-000000000000}';
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
    result := THashSHA2.GetHashString(s, SHA256);
end;

function TFormMain.CheckConfiguration: boolean;
var LConf: TWalletConfiguration;
    LFileName: string;
begin
  if rbTestnetWeb.IsChecked or rbMainnetWeb.IsChecked  then
    exit(true);
  // check local wallet: dms.conf
  LConf := TWalletConfiguration.CreateNew(true);
  // wallet does not seem to be configured for RPC
  if LConf.rpcuser.IsEmpty or LConf.rpcpassword.IsEmpty then begin
    if TDialogServiceSync.MessageDialog('Should DMS Core be configured for RPC access?',
              TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbYes, 0) = mrNo
    then
      exit(true);
    // create/update dms.conf
    result := false; // wallet restart requied
    LFileName := LConf.ConfFileName;
    with TStringList.Create do
    try
      if TFile.Exists(LFileName) then
        LoadFromFile(LFileName);
      Values['rpcuser'] := 'dmsrpcuser';
      Values['rpcpassword'] := 'dmsrpcpassword';
      if Values['rpcallowip'].IsEmpty then
        Values['rpcallowip'] := '127.0.0.1';
      Values['server'] := '1';
      SaveToFile(LFileName);
    finally
      Free;
    end;
    TDialogServiceSync.MessageDialog('Please restart DMS Core and then try again.',
            TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOk], TMsgDlgBtn.mbOk, 0);
  end
  else
    result := true;
end;

procedure TFormMain.btnStoreClick(Sender: TObject);
var LChain: TDocumentchain;
    LDoc  : TBlockchainDocument;
    LFileHash,
    LTxid : string;
begin
  if not CheckConfiguration then
    exit;
  if not OpenDlg.Execute then
    exit;
  edFileName.Text := OpenDlg.FileName;
  LFileHash := THashSHA2.GetHashStringFromFile(edFileName.Text, SHA512);
  lblFileHash.Text := 'Hash: ' + LFileHash;
  lblFileHash.Visible := true;

  LChain := TDocumentchain.Create(Self,
                                  rbTestnetWeb.IsChecked or rbMainnetWeb.IsChecked,
                                  rbTestnetWeb.IsChecked or rbTestnetLocal.IsChecked,
                                  ''); // you need a WebAPI account to store in mainnet
  try
    LDoc.FillData(GetGUID,
                  THashMD5.GetHashStringFromFile(edFileName.Text),
                  LFileHash,
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
    LFileHash,
    LTxid : string;
    LLog  : TStrings;
    LRes  : TAuditStatus;
    LMsgTp: TMsgDlgType;
begin
  if not TFile.Exists(edFileName.Text) then
    raise Exception.Create('File not found');
  LFileHash := THashSHA2.GetHashStringFromFile(edFileName.Text, SHA512);
  lblFileHash.Text := 'Hash: ' + LFileHash;
  lblFileHash.Visible := true;
  LLog := TStringList.Create;
  // current document
  LChain := TDocumentchain.Create(Self,
                                  rbTestnetWeb.IsChecked or rbMainnetWeb.IsChecked,
                                  rbTestnetWeb.IsChecked or rbTestnetLocal.IsChecked,
                                  ''); // account not needed for revision
  try
    LDoc.FillData(GetGUID,
                  THashMD5.GetHashStringFromFile(edFileName.Text),
                  LFileHash,
                  GetAttributeHash,
                  ''); // optional hash from publisher name
    LTxID := edTxid.Text; { TODO : use transaction id stored in your archive database }
    LDocBC := LChain.GetDocument(LTxID);
    // compare the documents
    LRes := TBlockchainDocument.CompareDocuments(LDoc, LDocBC, LLog);
    case LRes of
      auditOk       : LMsgTp := TMsgDlgType.mtInformation;
      auditHint     : LMsgTp := TMsgDlgType.mtWarning;
      auditViolation: LMsgTp := TMsgDlgType.mtError;
      else            LMsgTp := TMsgDlgType.mtError;
    end;
    TDialogServiceAsync.MessageDialog(LLog.Text, LMsgTp, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
  finally
    LLog.Free;
    LChain.Free;
  end;
end;

end.
