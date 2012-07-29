program fbconfig;
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

uses
  Forms,
  interfaces,
  Classes,
  LCLIntf,
  MainUnit in 'MainUnit.pas' {MainForm},
  ConfigLists in 'ConfigLists.pas',
  AliasProps in 'AliasProps.pas' {AliasPropForm},
  DefaultProps in 'DefaultProps.pas' {ConfigPropForm},
  PermissionProps in 'PermissionProps.pas' {PermissionPropForm},
  BooleanProps in 'BooleanProps.pas' {BooleanPropForm},
  DirectoryProps in 'DirectoryProps.pas' {DirectoryPropForm},
  ListProps in 'ListProps.pas' {ListPropForm},
  AboutForms in 'AboutForms.pas' {AboutForm};

{$R *.res}

begin
  Application.Title := 'FBConfig';
  Application.Initialize;
  Application.BidiMode := bdLeftToRight;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
