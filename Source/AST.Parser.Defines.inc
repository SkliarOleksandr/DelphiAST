///////////////////////////////////////
// IDE Version constants and defines //
///////////////////////////////////////

{$IFNDEF FPC}
  {$IF COMPILERVERSION >= 30}
    {$DEFINE XE+}
    {$DEFINE XE2+}
    {$DEFINE XE3+}
    {$DEFINE XE4+}
    {$DEFINE XE5+}
    {$DEFINE XE6+}
    {$DEFINE XE7+}
    {$DEFINE XE8+}
    {$DEFINE XE10+}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF FPC}
  {$MODE delphiunicode}
  {$IFDEF WINCE}
    {$DEFINE WINDOWS}
  {$ENDIF}
{$ENDIF}

{$IF Defined(CPUX86) or Defined(CPU86) or Defined(CPUARM)}
  {$DEFINE PTR_SIZE4}
{$IFEND}

{$IF Defined(CPUX64) or Defined(CPUX86_64)}
  {$DEFINE PTR_SIZE8}
{$IFEND}
