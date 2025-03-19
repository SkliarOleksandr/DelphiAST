unit ASTTest.Procs.External2;

interface

const
  shell32 = 'shell32.dll';
  

function SHQueryRecycleBin: Cardinal; stdcall;
function SHQueryRecycleBinA: Cardinal; stdcall;
function SHQueryRecycleBinW: Cardinal; stdcall;

implementation

function SHQueryRecycleBin; external shell32 name 'SHQueryRecycleBinW'
function SHQueryRecycleBinA; external shell32 name 'SHQueryRecycleBinA';
function SHQueryRecycleBinW; external shell32 name 'SHQueryRecycleBinW'

end.