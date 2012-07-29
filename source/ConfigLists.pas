unit ConfigLists;

{***********************************************************************

  Copyright (C) 2004  Jihad Khalifa (jihad@parmaja.com)

  This file is part of Parmaja tools.

  fbConfig is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation; either version 2 of the License,
  or (at your option) any later version.

  fbConfig is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
  MA  02111-1307  USA

************************************************************************}

interface

uses
  LCLIntf, SysUtils, Classes, Forms, Graphics, Controls,
  Dialogs, Contnrs, IniFiles;

type
  TItemKind = (ikNone, ikBoolean, ikDirectory, ikPermission, ikList);

  TConfigList = class;

  TConfigItem = class(TObject)
  private
    FList: TConfigList;
    FID: Integer;
    FIdent: string;
    FValue: string;
    FDefValue: string;
    FKind: TItemKind;
    FPickList: string;
    FGroup: Integer;
    FIsHashed: Boolean;
    procedure SetList(const Value: TConfigList);
    function Get_Text: string;
    procedure Set_Text(const Value: string);
    function Get_Value: string;
    procedure Set_Value(const Value: string);
    function Get_DefValue: string;
    procedure Set_DefValue(const Value: string);
  public
    destructor Destroy; override;
    property List: TConfigList read FList write SetList;
  published
    property ID: Integer read FID write FID;
    property Ident: string read FIdent write FIdent;
    property Value: string read Get_Value write Set_Value;
    property DefValue: string read Get_DefValue write Set_DefValue;
    property Text: string read Get_Text write Set_Text;
    property IsHashed: Boolean read FIsHashed write FIsHashed default True;
    property Kind: TItemKind read FKind write FKind;
    property PickList: string read FPickList write FPickList;
    property Group: Integer read FGroup write FGroup;
  end;

  TConfigList = class(TObjectList)
  private
    FGroups: TStringList;
  protected
    function GetItem(Index: Integer): TConfigItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItem: TConfigItem): Integer; overload;
    function Add(vStr: string; vGroup: Integer = 0): Integer; overload;
    function Remove(AItem: TConfigItem): Integer;
    function IndexOf(AItem: TConfigItem): Integer;
    function First: TConfigItem;
    function Last: TConfigItem;
    procedure Insert(Index: Integer; AItem: TConfigItem);
    procedure LoadFromFile(const vFileName: string);
    procedure UpdateValues(vList: TStringList);
    procedure UpdateList(vList: TStringList);
    property Items[Index: Integer]: TConfigItem read GetItem; default;
    property Groups: TStringList read FGroups write FGroups;
  end;

function GetPartStr(const vStr: string; vComma: char; vPart: integer; vQuoted: Char='"'): string;
function DeleteHash(var vStr: string): Boolean;

implementation

function GetPartStr(const vStr: string; vComma: char; vPart: integer; vQuoted: Char='"'): string;
var
  j, i, LastPos, l: integer;
  InQuoted: Boolean;
  b, c: integer;
begin
  i := 0;
  j := 0;
  LastPos := 1;
  l := length(vStr);
  InQuoted := False;
  while i < l do
  begin
    i := i + 1;
    if (vStr[i] = vComma) and (not InQuoted) then
    begin
      if j >= vPart then
      begin
        i := i - 1;
        break;
      end;
      j := j + 1;
      LastPos := i + 1;
    end
    else if (vQuoted <> #0) and (vStr[i] = vQuoted) then
      InQuoted := not InQuoted
  end;
  if j = vPart then
  begin
    b := LastPos;
    c := i - LastPos + 1;
    Result := Copy(vStr, b, c);
  end;
end;

function DeleteHash(var vStr: string): Boolean;
var
  I, c: integer;
begin
  c := 0;
  Result := False;
  for I := 1 to Length(vStr) do
  begin
    if vStr[I] = '#' then
    begin
      Inc(c);
      Result := True;
    end
    else
      Break;
  end;
  if c > 0 then
    Delete(vStr, 1, c);
end;

{ TConfigItem }

destructor TConfigItem.Destroy;
begin
  if FList <> nil then
    FList.Remove(Self);
  inherited;
end;

function TConfigItem.Get_DefValue: string;
begin
  if FKind = ikBoolean then
  begin
    if FDefValue = '1' then
      Result := 'True'
    else
      Result := 'False';
  end
  else
    Result := FDefValue;
end;

function TConfigItem.Get_Text: string;
begin
  Result := Ident + '=' + Value;
end;

function TConfigItem.Get_Value: string;
begin
  if FKind = ikBoolean then
  begin
    if FValue = '1' then
      Result := 'True'
    else
      Result := 'False';
  end
  else
    Result := FValue;
end;

procedure TConfigItem.SetList(const Value: TConfigList);
begin
  if FList <> Value then
  begin
    if Assigned(FList) then
      FList.Remove(Self);
    FList := Value;
    if Assigned(FList) then
      FList.Add(Self);
  end;
end;

procedure TConfigItem.Set_DefValue(const Value: string);
begin
  if FKind = ikBoolean then
  begin
    if DefValue = 'True' then
      FDefValue := '1'
    else
      FDefValue := '0';
  end
  else
    FDefValue := Value;
end;

procedure TConfigItem.Set_Text(const Value: string);
var
  S: string;
begin
  S := GetPartStr(Value, '=', 0);
  FIdent := Trim(S);
  S := GetPartStr(Value, '=', 1);
  FDefValue := Trim(S);
  FValue := FDefValue;
end;

procedure TConfigItem.Set_Value(const Value: string);
begin
  if FKind = ikBoolean then
  begin
    if Value = 'True' then
      FValue := '1'
    else
      FValue := '0';
  end
  else
    FValue := Value;
end;

{ TConfigList }

function TConfigList.Add(AItem: TConfigItem): Integer;
begin
  Result := inherited Add(AItem);
end;

function TConfigList.Add(vStr: string; vGroup: Integer): Integer;
var
  aItem: TConfigItem;
  S: string;
  P: Integer;
begin
  aItem := TConfigItem.Create;
  aItem.IsHashed := True;
  aItem.Kind := ikNone;
  aItem.Group := vGroup;
  aItem.ID := -1;
  P := Pos(';', vStr);
  if P = 0 then
    aItem.Text := vStr
  else
  begin
    S := GetPartStr(vStr, ';', 0);
    aItem.Text := S;
    S := GetPartStr(vStr, ';', 1);
    if S = 'd' then
      aItem.Kind := ikDirectory
    else if S = 'b' then
      aItem.Kind := ikBoolean
    else if S = 'p' then
      aItem.Kind := ikPermission
    else if S = 'l' then
    begin
      aItem.Kind := ikList;
      S := GetPartStr(vStr, ';', 2);
      aItem.PickList := S;
    end;
  end;
  Result := Add(aItem);
end;

constructor TConfigList.Create;
begin
  inherited Create;
  FGroups := TStringList.Create;
end;

destructor TConfigList.Destroy;
begin
  FGroups.Free;
  inherited;
end;

function TConfigList.First: TConfigItem;
begin
  Result := TConfigItem(inherited First);
end;

function TConfigList.GetItem(Index: Integer): TConfigItem;
begin
  Result := inherited Items[Index] as TConfigItem;
end;

function TConfigList.IndexOf(AItem: TConfigItem): Integer;
begin
  Result := inherited IndexOf(AItem);
end;

procedure TConfigList.Insert(Index: Integer; AItem: TConfigItem);
begin
  inherited Insert(Index, AItem);
end;

function TConfigList.Last: TConfigItem;
begin
  Result := TConfigItem(inherited Last);
end;

procedure TConfigList.LoadFromFile(const vFileName: string);
var
  aFile: TMemIniFile;
  aItems: TStringList;
  I, J: Integer;
begin
  aItems := TStringList.Create;
  aFile := TMemIniFile.Create(vFileName);
  aFile.ReadSections(FGroups);
  for I := 0 to FGroups.Count - 1 do
  begin
    aFile.ReadSectionValues(FGroups[I], aItems);
    for J := 0 to aItems.Count - 1 do
    begin
      Add(aItems[J], I + 1);
    end;
  end;
  aItems.Free;
  aFile.Free;
end;

function TConfigList.Remove(AItem: TConfigItem): Integer;
begin
  Result := inherited Remove(AItem);
end;

procedure TConfigList.UpdateList(vList: TStringList);
var
  I, Idx: Integer;
  S: string;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[I].IsHashed then
      S := '#'
    else
      S := '';
    Idx := Items[I].ID;
    if (Idx = -1) then
      vList.Add(S + Items[I].FIdent + '=' + Items[I].FValue)
    else
      vList[Idx] := S + Items[I].FIdent + '=' + Items[I].FValue;
  end;
end;

procedure TConfigList.UpdateValues(vList: TStringList);
var
  I, J: Integer;
  aIsHashed: Boolean;
  aStr, aIdent, aValue: string;
begin
  for I := 0 to vList.Count - 1 do
  begin
    aStr := vList[I];
    if Pos('=', aStr) <> 0 then
    begin
      aIsHashed := DeleteHash(aStr);
      aIdent := GetPartStr(aStr, '=', 0);
      aIdent := Trim(aIdent);
      for J := 0 to Count - 1 do
      begin
        if CompareStr(aIdent, Items[J].Ident) = 0 then
        begin
          if Items[J].IsHashed or not aIsHashed then
          begin
            Items[J].ID := I;
            aValue := GetPartStr(aStr, '=', 1);
            aValue := Trim(aValue);
            Items[J].FValue := aValue;
          end;
          Items[J].IsHashed := aIsHashed;
          Break;
        end;
      end;
    end;
  end;
end;

end.

 
