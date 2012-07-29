unit MainUnit;
{$mode objfpc}{$H+}
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
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ConfigLists, Registry, ExtCtrls, StdCtrls,
  ComCtrls, Menus, LResources, Buttons;

type

  { TMainForm }

  TMainForm = class(TForm)
    AddBtn: TButton;
    ApplyBtn: TButton;
    Button1: TButton;
    Button2: TButton;
    CancelBtn: TButton;
    EditBtn: TButton;
    PageControl: TPageControl;
    Panel1: TPanel;
    RemoveBtn: TButton;
    RestoreBtn: TButton;
    SaveBtn: TButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    AliasesList: TListView;
    ConfPopupMenu: TPopupMenu;
    SetDefaultValue1: TMenuItem;
    Panel3: TPanel;
    CommentMemo: TMemo;
    Splitter1: TSplitter;
    Panel2: TPanel;
    SearchLbl: TLabel;
    Label1: TLabel;
    SearchEdit: TEdit;
    GroupCbo: TComboBox;
    ConfigList: TListView;
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetDefaultValue1Click(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure AliasesListDblClick(Sender: TObject);
    procedure AliasesListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GroupCboChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ConfigListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ConfigListDblClick(Sender: TObject);
    procedure ConfigListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
  private
    ConfigDefaults: TConfigList;
    AliaseItems: TStringList;
    CommentItems: TStringList;
    ConfigFile: TStringList;
    cnfFileName: string;
    alsFileName: string;
    IsChanged: Boolean;
    function FindItem(S: string): TListItem;
    procedure FindIt;
    procedure ShowItemProps;
  public
    procedure ReadConfig;
    procedure SaveConfig;
    procedure ReadAliases;
    procedure SaveAliases;
    procedure ReadComment(Section: String);
    procedure FillConfigList(vGroup: Integer = 0);
    procedure FillGroupCombo(Items: TStringList);
  end;

var
  MainForm: TMainForm;

implementation

uses DefaultProps, BooleanProps, DirectoryProps, ListProps,
  PermissionProps, AliasProps, AboutForms;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ReadConfig;
var
  aFileName: string;
begin
  ConfigFile.LoadFromFile(cnfFileName);
  {$IFDEF WINDOWS}
  aFileName := ExpandFileName(IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName)) + 'defaults.txt');
  {$ELSE}
  aFileName := ExpandFileName(IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName)) + 'defaults.txt');
  {$ENDIF}
  ConfigDefaults.LoadFromFile(aFileName);
  ConfigDefaults.UpdateValues(ConfigFile);
  FillConfigList;
end;

procedure TMainForm.SaveConfig;
var
  idx, i: Integer;
begin
  for i := 0 to ConfigList.Items.Count-1 do
  begin
    Idx := Integer(ConfigList.Items[i].Data);
    IsChanged := IsChanged or (ConfigDefaults[Idx].IsHashed <> not ConfigList.Items[i].Checked);
    ConfigDefaults[Idx].IsHashed := not ConfigList.Items[i].Checked;
  end;
  ConfigDefaults.UpdateList(ConfigFile);
  ConfigFile.SaveToFile(cnfFileName);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ConfigDefaults.Free;
  ConfigFile.Free;
  AliaseItems.Free;
  CommentItems.Free;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  FindIt;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  {$IFDEF WINDOWS}
  aReg: TRegistry;
  {$ENDIF}
  aFileDir, aFileName: string;
begin
  ConfigDefaults := TConfigList.Create;
  ConfigFile := TStringList.Create;
  CommentItems := TStringList.Create;
  AliaseItems := TStringList.Create;
  {$IFDEF WINDOWS}
  aReg := TRegistry.Create;
  aReg.RootKey := HKEY_LOCAL_MACHINE;
  aReg.Access:=KEY_READ;
  if aReg.OpenKey('SOFTWARE\Firebird Project\Firebird Server\Instances', False) then
    aFileDir := aReg.ReadString('DefaultInstance');
  aReg.CloseKey;
  aReg.Free;
  {$ELSE}
  aFileDir := '/opt/firebird/';
  {$ENDIF}
  cnfFileName := aFileDir + 'firebird.conf';
  alsFileName := aFileDir + 'aliases.conf';
  ReadConfig;
  ReadAliases;
  FillGroupCombo(ConfigDefaults.Groups);
  {$IFDEF WINDOWS}
  aFileName := ExpandFileName(IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName)) + 'comments.txt');
  {$ELSE}
  aFileName := ExpandFileName(IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName)) + 'comments.txt');
  {$ENDIF}
  CommentItems.LoadFromFile(aFileName);
  PageControl.TabIndex := 0;
end;

procedure TMainForm.SaveBtnClick(Sender: TObject);
begin
  SaveConfig;
  SaveAliases;
  IsChanged := False;
  Close;
end;

procedure TMainForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ApplyBtnClick(Sender: TObject);
begin
  SaveConfig;
  SaveAliases;
  IsChanged := False;
end;

procedure TMainForm.SearchEditChange(Sender: TObject);
begin
end;

procedure TMainForm.FindIt;
var
  aItem: TListItem;
begin
  if SearchEdit.Text <> '' then
  begin
    aItem := FindItem(SearchEdit.Text);
    if aItem <> nil then
    begin
      ConfigList.Selected := aItem;
      ConfigList.Selected.MakeVisible(False);
    end
    else
      Beep;
  end;
end;

procedure TMainForm.SearchEditKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
  case Key of
    VK_DOWN, VK_UP: ConfigList.SetFocus;
    VK_RETURN: FindIt;
  end;
end;

procedure TMainForm.ReadComment(Section: String);
var
  Idx, I: Integer;
  T, S: string;
begin
  T := '';
  Idx := CommentItems.IndexOf('['+ Section +']');
  if Idx <> -1 then
  begin
    for I := Idx + 1 to CommentItems.Count - 1 do
    begin
      S := CommentItems[I];
      if (S <> '') and ((S[1] = '[') and (S[Length(S)] = ']')) then
        break
      else
        T := T + S + #13;
    end;
  end;
  CommentMemo.Lines.Text := T;
  CommentMemo.SelStart := 0;
end;

procedure TMainForm.SetDefaultValue1Click(Sender: TObject);
begin
  IsChanged := True;
end;

procedure TMainForm.ReadAliases;
var
  Line, S: string;
  I: Cardinal;
  aItem: TListItem;
begin
  AliaseItems.LoadFromFile(alsFileName);
  for I := 0 to AliaseItems.Count - 1 do
  begin
    Line := AliaseItems[I];
    Trim(Line);
    if (Line <> '') and (Line[1] <> '#') then
    begin
      aItem := AliasesList.Items.Add;
      S := GetPartStr(Line, '=', 0);
      Trim(S);
      aItem.Caption := S;
      aItem.Data := Pointer(I);
      S := GetPartStr(Line, '=', 1);
      Trim(S);
      aItem.SubItems.Add(S);
    end;
  end;
end;

procedure TMainForm.SaveAliases;
begin
  AliaseItems.SaveToFile(alsFileName);
end;

procedure TMainForm.RemoveBtnClick(Sender: TObject);
var
  I, Idx, Data: Integer;
begin
  if AliasesList.Selected <> nil then
  begin
    if MessageDlg('Delete Selected Item?', mtInformation, [mbYes,mbNo], 0) = mrYes then
    begin
      Idx := Integer(AliasesList.Selected.Data);
      AliaseItems.Delete(Idx);
      for I := 0 to AliasesList.Items.Count -1 do
      begin
        Data := Integer(AliasesList.Items[I].Data);
        if Data >= Idx then
          AliasesList.Items[I].Data := TObject(Data-1);
      end;
      AliasesList.Selected.Delete;
      IsChanged := True;
    end;
  end;
end;

procedure TMainForm.AddBtnClick(Sender: TObject);
var
  Idx: Integer;
  aItem: TListItem;
  aName, aPath: string;
begin
  aName := '';
  aPath := '';
  if ShowAliasProps(aName, aPath) then
  begin
    Idx := AliaseItems.Add(aName + '=' + aPath);
    aItem := AliasesList.Items.Add;
    aItem.Caption := aName;
    aItem.Data := Pointer(Idx);
    aItem.SubItems.Add(aPath);
    aItem.Checked := True;
    IsChanged := True;
  end;
end;

procedure TMainForm.EditBtnClick(Sender: TObject);
var
  Idx: Integer;
  aName, aPath: string;
begin
  if AliasesList.Selected <> nil then
  begin
    aName := AliasesList.Selected.Caption;
    aPath := AliasesList.Selected.SubItems[0];
    if ShowAliasProps(aName, aPath) then
    begin
      AliasesList.Selected.Caption := aName;
      AliasesList.Selected.SubItems[0] := aPath;
      Idx := Integer(AliasesList.Selected.Data);
      AliaseItems[Idx] := aName + '=' + aPath;
      IsChanged := True;
    end;
  end;
end;

procedure TMainForm.AliasesListDblClick(Sender: TObject);
begin
  EditBtnClick(Sender);
end;

procedure TMainForm.AliasesListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN: EditBtnClick(Sender);
    VK_DELETE: RemoveBtnClick(Sender);
  end;
end;

procedure TMainForm.FillGroupCombo(Items: TStringList);
var
  I: Integer;
begin
  GroupCbo.Clear;
  GroupCbo.AddItem('[All]', TObject(0));
  for I := 0 to Items.Count -1 do
  begin
    GroupCbo.AddItem(Items[I], TObject(I + 1));
  end;
  GroupCbo.ItemIndex := 0;
end;

procedure TMainForm.GroupCboChange(Sender: TObject);
begin
  FillConfigList(Integer(GroupCbo.Items.Objects[GroupCbo.ItemIndex]));
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  R: Integer;
begin
  if IsChanged then
  begin
    R := MessageDlg('Save Changes to file?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case R of
      mrYes:
        begin
          SaveConfig;
          SaveAliases;
        end;
      mrNo: CanClose := True;
      mrCancel: CanClose := False;
    end;
  end;
end;

procedure TMainForm.FillConfigList(vGroup: Integer);
var
  I: Integer;
  aItem: TListItem;
begin
  with ConfigList do
  begin
    BeginUpdate;
    Items.Clear;
    for I := 0 to ConfigDefaults.Count - 1 do
    begin
      if (vGroup = 0) or (ConfigDefaults[I].Group = vGroup) then
      begin
        aItem := Items.Add;
        aItem.Data := Pointer(I);
        aItem.Checked := not ConfigDefaults[I].IsHashed;
        aItem.Caption := ConfigDefaults[I].Ident;
        aItem.SubItems.Add(ConfigDefaults[I].Value);
      end;
    end;
    EndUpdate;
  end;
end;

procedure TMainForm.ConfigListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if (Item<>nil) and Selected then
    ReadComment(Item.Caption);
end;

procedure TMainForm.ConfigListDblClick(Sender: TObject);
begin
  ShowItemProps;
end;

procedure TMainForm.ShowItemProps;
var
  Idx: Integer; 
  aItem: TConfigItem;
  aChanged: Boolean;
begin
  if ConfigList.Selected <> nil then
  begin
    Idx := Integer(ConfigList.Selected.Data);
    aItem := ConfigDefaults[Idx];
    case aItem.Kind of
      ikBoolean: aChanged := ShowBooleanProps(aItem);
      ikDirectory: aChanged := ShowDirectoryProps(aItem);
      ikPermission: aChanged := ShowPermissionProps(aItem);
      ikList: aChanged := ShowListProps(aItem);
    else
      aChanged := ShowDefaultProps(aItem);
    end;
    if aChanged then
    begin
      ConfigDefaults[Idx].Value := aItem.Value;
      ConfigList.Selected.SubItems[0] := aItem.Value;
      ConfigList.Selected.Checked := not aItem.IsHashed;
      IsChanged := True;
    end;
  end;
end;

procedure TMainForm.ConfigListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN: ShowItemProps;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  with TAboutForm.Create(Application) do
  begin
    ShowModal;
    Free;
  end;
end;

function TMainForm.FindItem(S: string): TListItem;
  function FindNow(FromIndex, ToIndex: Integer): TListItem;
  var
    i: Integer;
  begin
    Result := nil;
    for i := FromIndex to ToIndex do
    begin
      if pos(S, UpperCase(ConfigList.Items[i].Caption)) > 0 then
      begin
        Result := ConfigList.Items[i];
        exit;
      end;
    end;
  end;
begin
  S := UpperCase(S);
  Result := FindNow(ConfigList.ItemIndex + 1, ConfigList.Items.Count - 1);
  if Result = nil then
    Result := FindNow(0 , ConfigList.ItemIndex - 1);
end;

end.

