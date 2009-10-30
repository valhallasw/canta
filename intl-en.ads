-- *****************
-- Version ENGLISH
-- *****************
package Intl is

   langue : constant string := "en";

   new_line : constant string := ( 1 => ascii.lf, 2 => ascii.cr );

   URL_Site : constant String := "http://www.singintune.org/";

   -- ----------------------------------------------------------------------------------
   demo : constant string := "DEMO";

   demo_text  : constant string := "This demo version will stop now." & new_line
                                    & "The full version is not time-limited." & new_line
                                    & "Click on the 'Web' button to buy the full version.";

   attente_demo_text1 : constant string := "You must wait";
   attente_demo_text2 : constant string := " before beeing able to restart the demo.";

   -- -------------------------------------------------------------------------------------------

   -- Message d'erreur à l'execution d'une clé invalide
   titre_invalide : constant String := "Invalid key !";

   suite_inval : constant string := "Connect to the web site " & URL_Site
                                      & " to get a valid key.";
   cle_invalide : constant String := "The authorization key is not valid." & new_line
                                      & suite_inval ;

   key_not_found : constant String := "The authorization key was not found !" & new_line & new_line
                                       & "You can download it with the link provided in the mail" & new_line
                                       & "sent to you when you bought the product." & new_line
                                       & "Copy it in Canta's installation directory."
                                       & new_line & new_line & "The validation code is: ";

   -- ----------------------------------------------------------------------------------------------
   -- Dialog pour la clé de validation

   -- bouton Copier pour la signature hard
   dlg_copier_txt  : constant String := "Copy";

   -- label signature
   dlg_sign_txt   : constant String := "Validation code:";

   -- version Demo:
   dlg_demo_title : constant string := "Demo version limitations";

   dlg_demo_intro : constant string := "This demo version is time-limited."
                                    & "It will stop after ten minutes of working "
                                    & "and you must wait ten more minutes to restart it."
                                    & "But all the features are here and you can test them freely." & new_line
                                    & new_line
                                    & "The full version is not time-limited, "
                                    & "you can buy it by clicking on the 'Web' button." & new_line
                                    & "For this you need the following validation code." & new_line
                                    & "You can copy it in the clipboard by clicking on the 'Copy' button.";


   -- version Full:
   dlg_auto_title : constant string := "Software not allowed";

   dlg_instruct_txt : constant String :=  "The authorization key is not valid." & new_line
                                      & new_line
                                      & suite_inval & new_line
                                      & new_line
                                      & "For this you need the following validation code." & new_line
                                      & "You can copy it in the clipboard by clicking on the 'Copy' button.";


   -- ------------------------------------------------------------------------------

   bouton_sortie_txt : constant string := "Exit";
   bouton_param_txt  : constant String := "Param.";
   bouton_aide_txt   : constant String := "Help";
   bouton_site_txt   : constant String := "Web";
   bouton_option_txt : constant String := "Options";

   bouton_load_txt  : constant String := "Load";
   bouton_play_txt  : constant String := "Play";
   bouton_stop_txt  : constant String := "Stop";
   bouton_record_txt  : constant String := "REC.";
   bouton_clear_txt : constant String := "Delete";
   bouton_freeze_txt : constant String := "Freeze";

   -- pour les filtres du dialog open midi
   fichier_midi     : constant string := "MIDI files (*.mid and *.kar)";
   Titre_dialog_open_midi : constant string := "Opening MIDI file" & ascii.nul;


   -- pour les filtres du dialog sauvegarde
--   fichier_wave     : constant string := "Fichiers WAVE (*.wav) et MIDI (*.mid)";
--   Titre_dialog_save : constant string := "Sauvegarde en MIDI et WAVE" & ascii.nul;
   fichier_wave     : constant string := "WAVE files (*.wav)";
   Titre_dialog_save : constant string := "Saving WAVE file" & ascii.nul;

   -- labels de l'interface
   freq_string  : constant String := "Hz";
   debut_player : constant String := "from:";
   fin_player   : constant String := "to:";
   Vol_melodie	: constant String := "M";
   Vol_accomp	: constant String := "A";
   Transpo	: constant String := "T:";
   Decalage	: constant String := "O:";

   -- nom des notes
   subtype string_4 is string(1..4);
   Nom_notes : constant array(0..11) of string_4 := (
--   "  Do", " Do#","  Ré", " Ré#", "  Mi", "  Fa", " Fa#", " Sol", "Sol#", "  La", " La#", "  Si");
     "   C", "  C#","   D", "  D#", "   E", "   F", "  F#", "   G", "  G#", "   A", "  A#", "   B");

   -- messages d'erreur
   def_err_title   : constant string := "Error !";
   err_exception   : constant string := "Fatal error ! Please read the file 'log.txt'.";
   --
   err_init_txt    : constant string := "Error during software initialization";
   err_win_txt     : constant string := "Error creating window ";
   err_class_txt   : constant string := "Error creating class ";
   err_class_title : constant string := "Error in the class creation";
   err_obj_txt     : constant string := "Error creating object Id=";
   --
   err_play_title  : constant string := "Error in the MIDI player";
   err_timer_txt   : constant string := "The timer could not be started";
   err_midi_title  : constant string := "Error in the MIDI device";
   err_midi_dev_txt: constant string := "No MIDI device could be found on this computer";
   -- erreur mixer
   err_no_mixer    : constant string := "No mixer device detected on this machine";
   err_mixer_open  : constant string := "It was impossible to open a mixer device";
   -- erreur pas de microphone
   err_micro_title : constant string := "No microphone !";
   err_micro_txt   : constant string := "No microphone detected on this machine !" & new_line
                                       & "Canta could no work without microphone." & new_line
                                       & new_line
                                       & "With some sound card it is mandatory to connect the micrhophone "
                                       & "before starting the software.";
   -- erreur shell
   err_lance_aide  : constant string := "It was not possible to open the help file.";
   err_lance_web   : constant string := "It was not possible to start you web browser.";

   -- ouverture fichier midi
   err_fnf_txt     : constant string := "File not found ";
   err_midi_txt    : constant string := "This file is not a valid MIDI file ";
   err_chant_txt   : constant string := "This midi file can not be used in this application.";

   -- création fichier wav
   err_cre_wav     : constant string := "Unable to create the file !" & new_line & new_line
                                         & "Check if it is not used by another application.";
   -- skins
   err_skin_not_found : constant String := "Unable to open the skin file:" & new_line;
   err_skin_syntax    : constant String := "There is an error in the skin file at line";
   err_bitmap_inval   : constant String := "This file is not a valid bitmap file" & new_line;
   err_bitmap_taille  : constant String := "Invalid header size" & new_line;
   err_bitmap_sens    : constant String := "Invalid top-down image" & new_line;
   err_bitmap_plan    : constant String := "Invalid number of planes" & new_line;
   err_bitmap_32bits  : constant string := "This bitmap is not in 32 bits" & new_line;
   err_bitmap_compres : constant string := "Invalid bitmap: compressed" & new_line;
   err_bitmap_dens_h  : constant String := "Horizontal density /= 72 dpi" & new_line;
   err_bitmap_dens_v  : constant string := "Vertical density /= 72 dpi" & new_line;
   err_bitmap_true    : constant String := "This bit maps is not in true color" & new_line;
   err_bitmap_read    : constant String := "Error while reading the bitmap" & new_line;
   err_skin_missing   : constant String := "Invalid skin, a file is missing for" & new_line;
   err_bitmap_notfound: constant String := "Missing bitmap file" & new_line;

   changement_skin : constant String := "The skin change will be effective at" & new_line
                                      & "the next startup of the application.";
   -- informations
   Info_titre : constant String := "Importante information";


   -- dialogues
   dlg_param_title : constant string := "Parameters selection";
   dlg_fs_title    : constant string := "Frequency selection";
   dlg_freq_txt    : constant string := "Frequency";
   dlg_micro_txt   : constant string := "Microphone";
   dlg_ok_txt      : constant string := "OK";
   dlg_cancel_txt  : constant string := "Cancel";
   dlg_track_title : constant string := "Melody track selection";
   dlg_select_txt  : constant string := "Select";
   dlg_midi_title  : constant string := "MIDI device selection";
   dlg_no_midi_txt : constant string := "No MIDI device on the computer";
   dlg_device_txt  : constant String := "MIDI device";
   -- Options
   dlg_auto_volume : constant String := "Automatic volume";
   dlg_change_instr: constant String := "Change instrument";
   dlg_famille    :  constant String := "Instrument families";
   dlg_instrument  : constant String := "Melody instruments";
   dlg_skin        : constant String := "Interface (skin)";
   dlg_oui_sz      : constant String := "Yes" & ascii.nul;
   dlg_non_sz      : constant String := "No" & ascii.nul;
   Option_dlg_title: constant String := "Options";

   dlg_piste_txt   : constant String := "TRACKS";
   dlg_instr_txt   : constant String := "INSTRUMENTS";
   dlg_nocontrol_title : constant String := "Microphone volume control";
   dlg_novolum_txt : constant String := "The software was not able to automaticaly identify a control for the microphone volume." & new_line
              & new_line
              & "However it is possible that this control could be done by the application ""Sound Volume"" which is part of the Windows system." & new_line
              & new_line
              & "If you want to launch this application just click on the button below:";
   dlg_lancer_txt : constant String := "Launch ""Sound Volume""";

   -- nom piste Midi
   Voix : constant String := "Voice";

   -- presse-papier
   clipboard_error : constant String := "The copy could not work if the clipboard is not closed.";

end Intl;
