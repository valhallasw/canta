with Win32;
with Win32.Winuser;

package Template_pkg is

   type template_type is private;

   type Class_type is ( Button, Edit, Static, List_box, Scroll_bar, Combo_box );

   function  New_template( Width, height : Win32.SHORT;
                           Titre : String) return template_type;

   procedure Add_Item( t: template_type;
                       X, Y , Width, Height: Win32.SHORT;
                       Id : Win32.WORD;
                       Titre : String;
                       Class : Class_type );


   function  Get_template( t : template_type ) return Win32.Winuser.LPCDLGTEMPLATEA;

   procedure Free( t : in out template_type );

private

    type template_buf_type is array(0..8192) of Win32.BYTE;

    type template_rec is record
        buffer    : template_buf_type := (others => 0);
        write_pos : Natural := 0;
    end record;

    type template_type is access template_rec;

end Template_pkg;
