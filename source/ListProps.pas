unit ListProps;
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
  TListPropForm = class(TForm)
    DefValueEdit: TEdit;
    ValueEdit: TComboBox;
    CancelBtn: TButton;
    OkBtn: TButton;
    Label2: TLabel;
    Label3: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function ShowListProps(var vItem: TConfigItem): Boolean;
   
implementation

{$R *.lfm}

function ShowListProps(var vItem: TConfigItem): Boolean;
var
  S: string;
begin
  with TListPropForm.Create(Application) do
  begin
    Caption := vItem.Ident + ' '+'Properties';
    DefValueEdit.Text := vItem.DefValue;
    S := vItem.PickList;
    S := StringReplace(S, ',', #13, [rfReplaceAll]);
    ValueEdit.Items.Text := S;
    ValueEdit.ItemIndex := ValueEdit.Items.IndexOf(vItem.Value);
    if ShowModal = mrOk then
    begin
      if ValueEdit.Text <> vItem.Value then
      begin
        vItem.Value := ValueEdit.Text;
        vItem.IsHashed := False;
      end;
      Result := True;
    end
    else
      Result := False;
  end;
end;

end.
