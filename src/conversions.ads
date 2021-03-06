with Unchecked_Conversion;
with System;

with Win32;
with Win32.Mmsystem;
with Win32.Winuser;
with Win32.Wingdi;
with Win32.Windef;
with Win32.Winnt;
with Win32.Winreg;
with Win32.Winbase;

with Common_Types; 	use Common_Types;

package Conversions is

   -- conversion d'Address -> Win32 type
   function TO_LPWNDCLASS      is new Unchecked_Conversion( System.Address, Win32.Winuser.ac_WNDCLASSA_t );
   function TO_LPCWAVEFORMATEX is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPCWAVEFORMATEX );
   function TO_LPSTR           is new Unchecked_Conversion( System.Address, Win32.LPSTR );
   function TO_ULONG           is new Unchecked_Conversion( System.Address, Win32.ULONG );
   function TO_DWORD           is new Unchecked_Conversion( System.Address, Win32.DWORD );
   function TO_ACPAINT         is new Unchecked_Conversion( System.Address, Win32.Winuser.ac_PAINTt );
   function TO_PRECT           is new Unchecked_Conversion( System.Address, Win32.Windef.PRECT );
   function TO_ACRECT          is new Unchecked_Conversion( System.Address, Win32.Wingdi.ac_RECT_t );
   function TO_ACMETRICS       is new Unchecked_Conversion( System.Address, Win32.Wingdi.ac_TEXTMETRIC_t );
   function TO_PCCH            is new Unchecked_Conversion( System.Address, Win32.PCCH );
   function TO_PCHAR           is new Unchecked_Conversion( System.Address, Win32.PCHAR );
   function TO_PWCH            is new Unchecked_Conversion( System.Address, Win32.PWCH );
   function TO_LPCDLGTEMPLATE  is new Unchecked_Conversion( System.Address, Win32.Winuser.LPCDLGTEMPLATEA );
   function To_PHKEY           is new Unchecked_Conversion( System.Address, Win32.Winreg.PHKEY);
   function To_PCBYTE          is new Unchecked_Conversion( System.Address, Win32.PCBYTE);
   function TO_LPARAM          is new Unchecked_Conversion( System.Address, Win32.LPARAM );
   function TO_LPRECT          is new Unchecked_Conversion( System.Address, Win32.Windef.LPRECT );
   function TO_ACBITMAPINFO    is new Unchecked_Conversion( System.Address, Win32.Wingdi.ac_BITMAPINFO_t );
   function To_LPCSTR          is new Unchecked_Conversion( System.Address, Win32.LPCSTR );
   function TO_LPTIMECAPS      is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPTIMECAPS );
   function TO_LPDWORD         is new Unchecked_Conversion( System.Address, Win32.LPDWORD );
   function TO_LPWFD           is new Unchecked_Conversion( System.Address, Win32.Winbase.LPWIN32_FIND_DATAA );
   --
   function TO_LPWAVEINCAPS    is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPWAVEINCAPSA );
   function TO_PMIXERCAPSA     is new Unchecked_Conversion( System.Address, Win32.Mmsystem.PMIXERCAPSA );
   function TO_LPHWAVEIN       is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPHWAVEIN );
   function TO_LPHMIXER        is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPHMIXER );
   function TO_LPHMIDIOUT      is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPHMIDIOUT );
   function TO_LPMIXERCONTROLDETAILS
                               is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPMIXERCONTROLDETAILS );
   function TO_LPMIXERLINE     is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPMIXERLINE );
   function TO_LPMIXERLINECONTROLSA
                               is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPMIXERLINECONTROLSA );
   function TO_LPMIXERCONTROLA is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPMIXERCONTROLA );
   --



   -- Win32 types -> Address
   function TO_ADDRESS         is new Unchecked_Conversion( Win32.PBYTE,          System.Address );

   -- UInt (32bits) -> Handle (address 32 bits)
   function TO_HWND is new Unchecked_Conversion( Win32.WPARAM, Win32.Winnt.HANDLE );
   -- LP -> LP
   function TO_LPCREATESTRUCT  is new Unchecked_Conversion( Win32.LPARAM, Win32.Winuser.LPCREATESTRUCT );


   -- conversion d'access
   -- pour les headers de buffer
   function TO_LPWAVEHDR is new Unchecked_Conversion( head_pt, Win32.Mmsystem.LPWAVEHDR );
   function TO_LPARAM   is new Unchecked_Conversion( head_pt, Win32.LPARAM );
   function TO_HEADPT  is new Unchecked_Conversion( Win32.LPARAM, head_pt);

   -- Handle = Adresse -> Integer
   function TO_INTEGER  is new Unchecked_Conversion( System.Address, Integer );

   -- Dword -> entiers
   function TO_INTEGER is new Unchecked_Conversion( Win32.DWORD, Integer );

   -- character -> Byte
   function TO_BYTE      is new Unchecked_Conversion( Character, Win32.BYTE );
   function TO_CHARACTER is new Unchecked_Conversion( Win32.CHAR, Character );

   -- extraction de mot dans un long
   function HI_WORD( L : Win32.LONG ) return Win32.SHORT;
   procedure Split_short( L : Win32.LONG; hi, low : out Win32.SHORT);

   -- conversion de casse dans une string
   function To_lower( s : string ) return string;

   -- transforme une string en C avec un nul pour d�signer la fin en une string Ada
   function To_Ada( c_string : string ) return string;

end Conversions;
