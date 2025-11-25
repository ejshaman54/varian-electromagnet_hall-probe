unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MagnetControl;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnSetField: TButton;
    EditDesiredB: TEdit;
    LabelDesiredB: TLabel;
    LabelHallV: TLabel;
    LabelField: TLabel;
    Timer1: TTimer;
    procedure BtnSetFieldClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FMag: TMagnetController;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FMag := TMagnetController.Create;
  if not FMag.Init('/dev/comedi0') then
  begin
    ShowMessage('Failed to initialize magnet controller (Comedi/DAQ).');
  end
  else
  begin
    Timer1.Interval := 200; // 5 Hz
    Timer1.Enabled  := True;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
  FreeAndNil(FMag);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  v, b: Double;
begin
  if FMag = nil then Exit;

  v := FMag.ReadHallVoltage;
  b := FMag.ReadFieldTesla;

  LabelHallV.Caption := Format('Hall voltage: %.4f V', [v]);
  LabelField.Caption := Format('Field: %.4f T', [b]);
end;

procedure TForm1.BtnSetFieldClick(Sender: TObject);
var
  desiredB: Double;
begin
  if FMag = nil then Exit;

  if TryStrToFloat(EditDesiredB.Text, desiredB) then
  begin
    FMag.SetFieldTesla(desiredB);
  end
  else
    ShowMessage('Invalid field value');
end;

end.
