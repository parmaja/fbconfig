unit AliasProps;
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
  Dialogs, StdCtrls, Buttons, LResources;

type

  { TAliasPropForm }

  TAliasPropForm = class(TForm)
    NameEdit: TEdit;
    PathEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    BrowseBtn: TButton;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure BrowseBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
            
function ShowAliasProps(var vName, vPath: string): Boolean;

implementation

{$R *.lfm}

function ShowAliasProps(var vName, vPath: string): Boolean;
begin
  with TAliasPropForm.Create(Application) do
  begin
    NameEdit.Text := vName;
    PathEdit.Text := vPath;
    if ShowModal = mrOk then
    begin
      vName := NameEdit.Text;
      vPath := PathEdit.Text;
      Result := True;
    end
    else
    begin
      vName := '';
      vPath := '';
      Result := False;
    end;
  end;
end;

procedure TAliasPropForm.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TAliasPropForm.FormCreate(Sender: TObject);
begin

end;

procedure TAliasPropForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TAliasPropForm.BrowseBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    PathEdit.Text := OpenDialog.FileName;
end;

end.
