program documentchain;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmMain in 'frmMain.pas' {FormMain},
  DMS.Api in 'DMS.Api.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
