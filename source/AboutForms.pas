unit AboutForms;

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
  Dialogs, StdCtrls, ExtCtrls, LResources, Buttons;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Image1: TImage;
    Memo1: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin

end;

procedure TAboutForm.Image1Click(Sender: TObject);
begin

end;

end.
