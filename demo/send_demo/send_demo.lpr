program send_demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  smsSendDemoMainUnit,
  hlpAboutUnit,
  smssendParamsUnit;

{$R send_demo.res}

begin
  Application.Initialize;
  Application.CreateForm(TsmsSendDemoMainForm, smsSendDemoMainForm);
  Application.Run;
end.

