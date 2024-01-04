{$mode objfpc}

Program hello;

Type 
  TMyClass = Class
    Name: RawByteString;
    Procedure SetName (Const s: RawByteString);
    Procedure PrintName;
  End;

Procedure TMyClass.PrintName;
Begin
  writeLn(Name)
End;

Procedure TMyClass.SetName (Const s: RawByteString);
Begin
  Name := s
End;

Var 
  MyObj: TMyClass;
Begin
  MyObj := TMyClass.Create;
  MyObj.SetName('Hello, 世界 🏳️‍⚧️');
  MyObj.PrintName();
End.
