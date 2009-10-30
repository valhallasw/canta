with System;
with Win32.Winnt;
with Win32.Windef;

with Common_types;	use Common_types;
with Resources_pkg;	use Resources_pkg;

package Objects_pkg is

   type object_kind is ( Fader, Button, Text, Display, Bistable, Num_edit );
   type Button_state_type is ( but_Off, but_On, but_Clic, but_Inval );
   type fader_state_type is ( fader_Off, fader_On, fader_Inval );
   type text_cadrage is ( gauche, centre, droite );

   type frame_pos is ( corner_lt, border_t, corner_rt, border_r, corner_rb, border_b, corner_lb, border_l );
   type frame_set is array(frame_pos) of Resources_pkg.bmap_id_type;
   null_frame : constant frame_set := (others => Resources_pkg.NULL_ID );

   type UI_object_type;
   type UI_object_ptr is access UI_object_type;

   -- objet d'interface
   type UI_object_type( kind : object_kind ) is record
      id      : Resources_pkg.obj_id_type;		-- identifiant
      hwnd    : Win32.Winnt.HANDLE := N_A;		-- handle Windows
      DC      : Win32.Windef.HDC   := N_A;		-- Device Context propre
      X       : integer := 0;				-- position dans la main
      Y       : integer := 0;
      largeur : integer := 0;				-- taille totale
      hauteur : integer := 0;
      inner_w : integer := 0;				-- taille utile
      inner_h : integer := 0;
      --
      framed  : boolean   := false;			-- avec ou sans bordure
      frames  : frame_set := null_frame;		-- les bordures
      bord_x  : integer := 0;				-- décalage du à la bordure
      bord_y  : integer := 0;
      resize  : boolean   := false;			-- peut-on lui changer sa taille ?
      --
      brush   : Win32.Windef.HBRUSH   := N_A;
      color   : Win32.Windef.COLORREF := 0;
      font    : Win32.Windef.HFONT    := N_A;
      --
      memDC   : Win32.Windef.HDC     := N_A;		-- DC pour écriture en mémoire
      bitmap  : Win32.Windef.HBITMAP := N_A;		-- bitmap mémoire associée
      bits    : System.Address       := N_A;		-- adresse des bits du bitmap mémoire
      --
      next    : UI_object_ptr;
      --
      case kind is
         when Fader =>
            fader_state : fader_state_type := fader_off;	-- état du fader
            horiz       : boolean := false;			-- horizontal / vertical
            extrem_1,
            middle,
            extrem_2    : bmap_id_type := NULL_ID;		-- bitmaps du fond
            curs_off,
            curs_on,
            curs_inval  : bmap_id_type := NULL_ID;		-- bitmaps du curseur
            curs_x,
            curs_y      : integer := 0;				-- position du curseur
            mouse       : integer := 0;				-- décalage de la souris en mode capture
            curs_large,
            curs_haut   : integer := 0;				-- dimensions du curseur
            min_1,
            min_2       : integer := 0;				-- zone de déplacement du curseur
            min_val,
            max_val,						-- min et max associé
            value       : integer := 0;				-- valeur courante
            d1, d2      : integer := 0;				-- dimensions des bitmaps des extrémités

         when Button =>
            but_state  : button_state_type := but_off;		-- état
            fond_off,
            fond_on,
            fond_clic,
            fond_inval : bmap_id_type := NULL_ID;		-- bitmaps du fond
            but_text   : string_pt;				-- le texte si /= null
            but_map    : bmap_id_type := NULL_ID;		-- la bitmap si texte = null
            but_rect   : Win32.Windef.RECT;			-- décentrage du texte ou du bitmap

         when Text =>
            buffer : string_pt;					-- buffer pourle texte
            length : natural := 0;				-- longueur du texte à afficher
            cadrage: text_cadrage := gauche;			-- cadrage à gauche, centre ou à droite

         when Display =>
            null;

         when Bistable =>
            state_on   : boolean := false;			-- état, off par defau
            on_bitmap  : bmap_id_type := NULL_ID;		-- bitmap de l'état on
            off_bitmap : bmap_id_type := NULL_ID;		-- bitmap de l'état off

         when Num_edit =>
            capture_on: boolean := false;			-- se souvenir si en mode de capture de souris
            number    : integer := 0;				-- le chiffre
            base      : integer := 0;				-- base de calcule qd déplacement souris
            moved     : boolean := false;			-- flag pour clic ou drag
            max_num   : integer := 0;				-- valeur max autorisée
            min_num   : integer := 0;				-- valeur min autorisée
            max_digit : natural := 1;				-- nombre max de digit pour l'affichage

      end case;
   end record;

   -- ***********************************************************************************

   -- retrouve l'objet associé à l'identifiant Id
   function Object_of( Id : Resources_pkg.obj_id_type ) return UI_object_ptr;

   -- retrouve l'objet associé à la window hwnd
   function Object_of( hwnd  : Win32.Winnt.HANDLE ) return UI_object_ptr;

   -- création de la class pour tous les objets
   procedure Create_Objects_Class;

   -- ********************************************************************************

   -- création d'un Fader ou d'un ascenseur
   procedure Create_Fader( Id         : Resources_pkg.obj_id_type;
                           horiz      : boolean;
                           resizeable : boolean;
                           fond_1,
                           fond_2,
                           fond_3     : Resources_pkg.bmap_id_type;
                           curs_1,
                           curs_2,
                           curs_3     : Resources_pkg.bmap_id_type;
                           min, max   : integer;
                           offset_1,
                           offset_2   : integer );

   -- création d'un bouton texte
   procedure Create_Button_Text( Id         : Resources_pkg.obj_id_type;
                                 fond_off,
                                 fond_on,
                                 fond_clic,
                                 fond_inval : Resources_pkg.bmap_id_type;
                                 text       : string;
                                 font       : Win32.Windef.HFONT;
                                 color      : Win32.Windef.COLORREF;
                                 offset_x,
                                 offset_y   : integer );

   -- Création d'un bouton bitmap
   procedure Create_Button_Bitmap( Id         : Resources_pkg.obj_id_type;
                                   fond_off,
                                   fond_on,
                                   fond_clic,
                                   fond_inval : Resources_pkg.bmap_id_type;
                                   icone      : bmap_id_type;
                                   offset_x,
                                   offset_y   : integer );

   -- création d'une zone de texte pour affichage ou label
   procedure Create_Text( Id           : Resources_pkg.obj_id_type;
                          framed       : boolean;
                          frames       : frame_set;
                          inner_width,
                          inner_height : integer;
                          max_char     : integer;
                          font         : Win32.Windef.HFONT;
                          color        : Win32.Windef.COLORREF;
                          brush        : Win32.Windef.HBRUSH;
                          cadrage      : text_cadrage );

   -- création d'un display
   procedure Create_Display( Id          : Resources_pkg.obj_id_type;
                            framed       : boolean;
                            frames       : frame_set;
                            inner_width,
                            inner_height : integer;
                            brush        : Win32.Windef.HBRUSH;
                            resizeable   : boolean );

   -- création d'objet bistable
   procedure Create_Bistable( Id                : Resources_pkg.obj_id_type;
                              fond_off, fond_on : Resources_pkg.bmap_id_type );

   procedure Create_Num_edit( Id        : Resources_pkg.obj_id_type;
                              max_digit : natural;
                              font      : Win32.Windef.HFONT;
                              color     : Win32.Windef.COLORREF;
                              brush     : Win32.Windef.HBRUSH;
                              framed    : boolean;
                              frames    : frame_set;
                              min,
                              max,
                              value     : integer := 0 );


   -- ***********************************************************************************

   -- modifie les objets apres un resize : alloue nouveau bitmap aux dysplay
   procedure Resize_all;

   -- recalcule les taille intérieure quand on a modifié les tailles extérieures
   procedure Compute_Inner( obj : UI_object_ptr );

   -- Redimensionne Fader crée avec taille incorrecte
   procedure Resize_fader( obj : UI_object_ptr );

   -- met à jour les objets après un changement de bitmap
   procedure Change_all_bitmaps;


   -- affichage d'un objet désigné par son Id
   procedure Display( Id : Resources_pkg.obj_id_type );

   -- défini la couleur de police d'un objet
   procedure Set_Color( Id : Resources_pkg.obj_id_type; color : Win32.Windef.COLORREF );

   -- définie la police d'un objet
   procedure Set_Font( Id : Resources_pkg.obj_id_type; font : Win32.Windef.HFONT );

   -- ------------------------------------------------------------------------------------

   -- change la valeur d'un fader, le curseur est repositionné
   procedure Set_Fader_value( Id : Resources_pkg.obj_id_type; new_value : integer );

   -- retorune la valeur courante du fader
   function Fader_value( Id : Resources_pkg.obj_id_type ) return integer;

   -- assigne la zone de déplacement du curseur et le repositionne
   procedure Set_Fader_geometry( Id : Resources_pkg.obj_id_type; min_1, min_2 : integer );

   -- assigne les valeurs minimale et maximale d'un fader
   procedure Fader_Set_Min_Max( Id : Resources_pkg.obj_id_type;
                                min_val, max_val : integer );

   -- ------------------------------------------------------------------------------------

   -- retourne l'état de validité d'un bouton (utilisé par Freeze)
   function Is_valid( Id : Resources_pkg.obj_id_type ) return boolean;

   -- modifie le rectangle d'affichage d'un bouton avec des offsets
   procedure Set_button_rect( Id : Resources_pkg.obj_id_type; offset_x, offset_y : integer );

   -- ------------------------------------------------------------------------------------

   -- modifie le contenu d'un objet text
   procedure Set_Text( Id : Resources_pkg.obj_id_type; text : string );

   -- recalcule les dimensions de l'objet texte pour qu'il contienne exactement la string
   procedure Adjuste_text_to_string( Id : Resources_pkg.obj_id_type );

   -- ------------------------------------------------------------------------------------

   -- affichage d'un display
   procedure Display_display( obj : UI_object_ptr );

   -- rend valide un objet bouton ou fader
   procedure Validate( Id : Resources_pkg.obj_id_type );

   -- met dans l'état invalide le bouton ou le fader
   procedure Invalidate( Id : Resources_pkg.obj_id_type );

   -- ------------------------------------------------------------------------------------

   -- retourne l'état d'un bistable: true/false
   function Bistable_on( Id : Resources_pkg.obj_id_type ) return boolean;

   -- ------------------------------------------------------------------------------------

   -- Num_edit

   function Num_Get_value( Id : Resources_pkg.obj_id_type ) return integer;

   procedure Num_Set_Min( Id : Resources_pkg.obj_id_type;
                          min_value : integer );
   procedure Num_Set_Max( Id : Resources_pkg.obj_id_type;
                          max_value : integer );


   procedure Num_Set_value( Id : Resources_pkg.obj_id_type;
                            value : integer );

   procedure Display_Num_edit( Id : Resources_pkg.obj_id_type );

end Objects_pkg;
