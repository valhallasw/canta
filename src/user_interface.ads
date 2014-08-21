package User_Interface is

   procedure Init_UI;

   procedure Init_Instance;

   procedure Display_all( volume   : Short_Integer;
                          pitch    : Long_Float;
                          Note, Octave : Natural;
                          Index    : Integer );

   procedure Start_appli;

   procedure Stop_appli;

   -- invalide la player MIDI lorsque qu'aucun device n'est trouvé
   procedure Invalidate_player;

end User_Interface;
