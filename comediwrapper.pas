unit ComediWrapper;

{$mode objfpc}{$H+}

interface

type
  Pcomedi_t = Pointer;

  lsampl_t = Cardinal;

  Pcomedi_range = ^comedi_range;
  comedi_range = record
    min  : Double;
    max  : Double;
    unit_: LongInt;
  end;

const
  COMEDI_SUBD_AI = 0;
  COMEDI_SUBD_AO = 1;

  AREF_GROUND = 0;
  AREF_COMMON = 1;
  AREF_DIFF   = 2;

function comedi_open(const filename: PChar): Pcomedi_t; cdecl; external 'comedi';
function comedi_close(dev: Pcomedi_t): LongInt; cdecl; external 'comedi';

function comedi_find_subdevice_by_type(dev: Pcomedi_t; subdev_type: LongInt;
  subdev: LongInt): LongInt; cdecl; external 'comedi';

function comedi_get_range(dev: Pcomedi_t; subdev, chan, range: LongInt
  ): Pcomedi_range; cdecl; external 'comedi';

function comedi_get_maxdata(dev: Pcomedi_t; subdev, chan: LongInt
  ): lsampl_t; cdecl; external 'comedi';

function comedi_data_read(dev: Pcomedi_t; subdev, chan, rng, aref: LongInt;
  data: PLongWord): LongInt; cdecl; external 'comedi';

function comedi_data_write(dev: Pcomedi_t; subdev, chan, rng, aref: LongInt;
  data: LongWord): LongInt; cdecl; external 'comedi';

function comedi_to_phys(data: lsampl_t; range: Pcomedi_range;
  maxdata: lsampl_t): Double; cdecl; external 'comedi';

function comedi_from_phys(phys: Double; range: Pcomedi_range;
  maxdata: lsampl_t): lsampl_t; cdecl; external 'comedi';

procedure comedi_perror(const s: PChar); cdecl; external 'comedi';

implementation

end.
