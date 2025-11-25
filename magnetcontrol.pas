unit MagnetControl;

{$mode objfpc}{$H+}

interface

uses
  ComediWrapper;

type
  TMagnetController = class
  private
    FDev: Pcomedi_t;
    FAISubdev: LongInt;
    FAOSubdev: LongInt;

    // Cached range info
    FHallRange: Pcomedi_range;
    FHallMaxData: lsampl_t;

    FKEPCORange: Pcomedi_range;
    FKEPCOMaxData: lsampl_t;

  public
    constructor Create;
    destructor Destroy; override;

    function Init(const ADevice: PChar = '/dev/comedi0'): Boolean;

    function ReadHallVoltage: Double;
    function ReadFieldTesla: Double;

    procedure SetFieldTesla(DesiredB: Double);

  end;

// --- Calibration constants: EDIT THESE FOR YOUR SYSTEM ---

const
  // Hall probe calibration: B[T] = (V - HALL_OFFSET_V) * HALL_SENS_T_PER_V
  HALL_OFFSET_V        = 0.0;       // TODO: offset voltage at B = 0 T
  HALL_SENS_T_PER_V    = 1.0;       // TODO: Tesla per Volt from datasheet/calib

  // Magnet coil calibration: B[T] = COIL_T_PER_A * I[A]
  COIL_T_PER_A         = 0.1;       // TODO: Tesla per Amp (or T/A from magnet spec)

  // Kepco programming: I[A] = KEPCO_A_PER_V * V_program
  KEPCO_A_PER_V        = 2.0;       // TODO: Amps per Volt (e.g. 2A/V -> 10 V = 20 A)

implementation

uses
  SysUtils;

constructor TMagnetController.Create;
begin
  inherited Create;
  FDev        := nil;
  FAISubdev   := -1;
  FAOSubdev   := -1;
  FHallRange  := nil;
  FKEPCORange := nil;
end;

destructor TMagnetController.Destroy;
begin
  if Assigned(FDev) then
    comedi_close(FDev);
  inherited Destroy;
end;

function TMagnetController.Init(const ADevice: PChar): Boolean;
var
  hallChan, hallRangeIndex: LongInt;
  kepcoChan, kepcoRangeIndex: LongInt;
begin
  Result := False;

  FDev := comedi_open(ADevice);
  if FDev = nil then
  begin
    comedi_perror('comedi_open');
    Exit;
  end;

  // Find AI and AO subdevices
  FAISubdev := comedi_find_subdevice_by_type(FDev, COMEDI_SUBD_AI, 0);
  FAOSubdev := comedi_find_subdevice_by_type(FDev, COMEDI_SUBD_AO, 0);

  if (FAISubdev < 0) or (FAOSubdev < 0) then
  begin
    WriteLn('Could not find AI or AO subdevice');
    Exit;
  end;

  // Assume hall probe is on AI channel 0, range 0
  hallChan        := 0;
  hallRangeIndex  := 0;
  FHallRange      := comedi_get_range(FDev, FAISubdev, hallChan, hallRangeIndex);
  FHallMaxData    := comedi_get_maxdata(FDev, FAISubdev, hallChan);

  // Assume Kepco programming on AO channel 0/1, use range 0 for now
  kepcoChan       := 0;
  kepcoRangeIndex := 0;
  FKEPCORange     := comedi_get_range(FDev, FAOSubdev, kepcoChan, kepcoRangeIndex);
  FKEPCOMaxData   := comedi_get_maxdata(FDev, FAOSubdev, kepcoChan);

  Result := True;
end;

function TMagnetController.ReadHallVoltage: Double;
var
  data: LongWord;
  hallChan, hallRangeIndex: LongInt;
begin
  Result := 0.0;

  if not Assigned(FDev) or (FAISubdev < 0) then Exit;

  hallChan        := 0;
  hallRangeIndex  := 0;

  if comedi_data_read(FDev, FAISubdev, hallChan, hallRangeIndex,
                      AREF_GROUND, @data) < 0 then
  begin
    comedi_perror('comedi_data_read(Hall)');
    Exit;
  end;

  Result := comedi_to_phys(lsampl_t(data), FHallRange, FHallMaxData);
end;

function TMagnetController.ReadFieldTesla: Double;
var
  v: Double;
begin
  v       := ReadHallVoltage;
  Result  := (v - HALL_OFFSET_V) * HALL_SENS_T_PER_V;
end;

procedure TMagnetController.SetFieldTesla(DesiredB: Double);
var
  Icoil, Vprog: Double;
  kepcoChan0, kepcoChan1, kepcoRangeIndex: LongInt;
  aoValue: lsampl_t;
begin
  if not Assigned(FDev) or (FAOSubdev < 0) then Exit;

  // Convert desired B -> current -> programming voltage
  Icoil := DesiredB / COIL_T_PER_A;
  Vprog := Icoil / KEPCO_A_PER_V;

  // Clamp voltage to Kepco ±10 V
  if Vprog > 10.0 then Vprog := 10.0;
  if Vprog < -10.0 then Vprog := -10.0;

  // Convert phys -> raw
  aoValue := comedi_from_phys(Vprog, FKEPCORange, FKEPCOMaxData);

  kepcoChan0       := 0;
  kepcoChan1       := 1;
  kepcoRangeIndex  := 0;

  // Write to both Kepcos (two Kepco 20-20 in series)
  if comedi_data_write(FDev, FAOSubdev, kepcoChan0, kepcoRangeIndex,
                       AREF_GROUND, aoValue) < 0 then
    comedi_perror('comedi_data_write(Kepco0)');

  if comedi_data_write(FDev, FAOSubdev, kepcoChan1, kepcoRangeIndex,
                       AREF_GROUND, aoValue) < 0 then
    comedi_perror('comedi_data_write(Kepco1)');
end;

end.
