unit PermissionProps;
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
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ConfigLists, LResources, Buttons;

type
  TPermissionPropForm = class(TForm)
    DefValueEdit: TEdit;
    ValueEdit: TComboBox;
    CancelBtn: TButton;
    OkBtn: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    AddBtn: TButton;
    ParametersEdit: TEdit;
    procedure AddBtnClick(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function ShowPermissionProps(var vItem: TConfigItem): Boolean;

implementation

{$R *.lfm}

function ShowPermissionProps(var vItem: TConfigItem): Boolean;
var
  S: string;
begin
  with TPermissionPropForm.Create(Application) do
  begin
    Caption := vItem.Ident + ' ' + 'Properties';
    DefValueEdit.Text := vItem.DefValue;
    if Pos('Restrict', vItem.Value) = 1 then
    begin
      ValueEdit.ItemIndex := ValueEdit.Items.IndexOf('Restrict');
      S := vItem.Value;
      S := Copy(S, Length('Restrict') + 1, MaxInt);
      ParametersEdit.Text := Trim(S);
    end
    else
      ValueEdit.ItemIndex := ValueEdit.Items.IndexOf(vItem.Value);
    ValueEditChange(nil);
    if ShowModal = mrOk then
    begin
      S := ValueEdit.Text;
      if S = 'Restrict' then
        S := S + ' ' + ParametersEdit.Text;
      if S <> vItem.Value then
      begin
        vItem.Value := S;
        vItem.IsHashed := False;
      end;
      Result := True;
    end
    else
      Result := False;
  end;
end;

procedure TPermissionPropForm.AddBtnClick(Sender: TObject);
var
  aDir: string;
  P: Integer;
begin
  aDir := ParametersEdit.Text;
  P := Pos(';', aDir);
  if P > 0 then
    aDir := Copy(aDir, 1, P - 1);
  if SelectDirectory('Select Directory', '', aDir, false) then
  begin
    if ParametersEdit.Text = '' then
      ParametersEdit.Text := aDir
    else
      ParametersEdit.Text := ParametersEdit.Text + ';' + aDir;
  end;
end;

procedure TPermissionPropForm.ValueEditChange(Sender: TObject);
begin
  if ValueEdit.Text = 'Restrict' then
  begin
    ParametersEdit.Enabled := True;
    AddBtn.Enabled := True;
  end
  else
  begin
    ParametersEdit.Enabled := False;
    AddBtn.Enabled := False;
  end;
end;

end.

