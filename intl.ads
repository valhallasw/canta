package Intl is

   langue : constant string := "fr";

   new_line : constant string := ( 1 => ascii.lf, 2 => ascii.cr );

   URL_Site : constant String := "http://www.chaumetsoftware.com/";

   -- ----------------------------------------------------------------------------------
   demo : constant string := "DEMO";

   demo_text  : constant string := "Cette version de démonstration va s'arrêter maintenant." & new_line
                                    & "La version complète n'est pas limitée dans le temps." & new_line
                                    & "Cliquez sur le bouton 'Web' pour acheter la version complète.";

   attente_demo_text1 : constant string := "Vous devez attendre";
   attente_demo_text2 : constant string := " avant de pouvoir relancer la démo.";

   demo_finie : constant string := "Démonstration terminée";

   -- -------------------------------------------------------------------------------------------

   -- Message d'erreur à l'execution d'une clé invalide
   titre_invalide : constant String := "Clé invalide !";

   suite_inval : constant string := "Veuillez vous connectez sur le site " & URL_Site
                                      & " pour obtenir une clé valide.";
   cle_invalide : constant String := "La clé d'autorisation n'est pas valide." & new_line
                                      & suite_inval ;

   key_not_found : constant String := "La clé de validation n'a pas été trouvée." & new_line
                                       & "Vous pouvez la téléchargée grace au le lien situé dans le mail qui vous a été envoyé lors de votre achat." & new_line
                                       & "Veuillez la copier dans le répertoire d'installation de Canta."
                                       & new_line & new_line & "Code de validation: ";

   -- ----------------------------------------------------------------------------------------------
   -- Dialog pour la clé de validation

   -- bouton Copier pour la signature hard
   dlg_copier_txt  : constant String := "Copier";

   -- label signature
   dlg_sign_txt   : constant String := "Code de validation:";

   -- version Demo:
   dlg_demo_title : constant string := "Limitation de la version de démonstration";

   dlg_demo_intro : constant string := "Cette version de démonstration est limitée en temps."
                                    & "Elle va s'arrêter au bout de dix minutes de fonctionnement"
                                    & "et ne poura être relancée qu'après dix minutes de pause."
                                    & "Par contre, toutes les fonctions sont présentes et vous pouvez les tester librement." & new_line
                                    & new_line
                                    & "La version complète n'est pas limitée dans le temps, "
                                    & "vous pouvez l'acheter en cliquant sur le bouton 'Web'." & new_line
                                    & "Pour cela vous aurez besoin du code de validation ci-dessous." & new_line
                                    & "Vous pouvez le copier dans le presse-papier en cliquant sur le bouton 'Copier'.";


   -- version Full:
   dlg_auto_title : constant string := "Programme non autorisé";

   dlg_instruct_txt : constant String :=  "La clé d'autorisation n'est pas valide." & new_line
                                      & new_line
                                      & suite_inval & new_line
                                      & new_line
                                      & "Pour cela vous aurez besoin du code de validation ci-dessous." & new_line
                                      & "Vous pouvez le copier dans le presse-papier en cliquant sur le bouton 'Copier'.";


   -- ------------------------------------------------------------------------------

   -- texte des boutons
   bouton_sortie_txt : constant string := "Sortie";
   bouton_param_txt  : constant String := "Param.";
   bouton_aide_txt   : constant String := "Aide";
   bouton_site_txt   : constant String := "Web";
   bouton_option_txt : constant String := "Options";

   --
   bouton_load_txt   : constant String := "Load";
   bouton_play_txt   : constant String := "Play";
   bouton_stop_txt   : constant String := "Stop";
   bouton_record_txt : constant String := "REC.";
   bouton_clear_txt  : constant String := "Efface";
   bouton_freeze_txt : constant String := "Geler";

   -- pour les filtres du dialog open midi
   fichier_midi     : constant string := "Fichiers MIDI (*.mid et *.kar)";
   Titre_dialog_open_midi : constant string := "Ouverture du fichier MIDI" & ascii.nul;


   -- pour les filtres du dialog sauvegarde
--   fichier_wave     : constant string := "Fichiers WAVE (*.wav) et MIDI (*.mid)";
--   Titre_dialog_save : constant string := "Sauvegarde en MIDI et WAVE" & ascii.nul;
   fichier_wave     : constant string := "Fichiers WAVE (*.wav)";
   Titre_dialog_save : constant string := "Sauvegarde en WAVE" & ascii.nul;

   -- labels de l'interface
   Freq_string  : constant String := "Hz";
   Debut_player : constant String := "début:";
   Fin_player   : constant String := "fin:";
   Vol_melodie	: constant String := "M";
   Vol_accomp	: constant String := "A";
   Transpo	: constant String := "T:";
   Decalage	: constant String := "D:";


   -- nom des notes
   subtype string_4 is string(1..4);
   Nom_notes : constant array(0..11) of string_4 := (
   "  Do", " Do#","  Ré", " Ré#", "  Mi", "  Fa", " Fa#", " Sol", "Sol#", "  La", " La#", "  Si");

   -- messages d'erreur
   def_err_title   : constant string := "Erreur !";
   err_exception   : constant string := "Une erreur fatale s'est produite ! Veuillez consultez le fichier 'log.txt'.";

   err_init_txt    : constant string := "Erreur lors de l'initialisation du programme";
   err_win_txt     : constant string := "Erreur de création de la window ";
   err_class_txt   : constant string := "Erreur dans la création de la class ";
   err_class_title : constant string := "Erreur de création de classe";
   err_obj_txt     : constant string := "Erreur dans la création de l'objet Id=";
   -- erreur MIDI device
   err_play_title  : constant string := "Erreur du player MIDI";
   err_timer_txt   : constant string := "Le timer n'a pas pu être démarré";
   err_midi_title  : constant string := "Erreur de device MIDI";
   err_midi_dev_txt: constant string := "Aucun device MIDI identifié sur cette machine";
   -- erreur mixer
   err_no_mixer    : constant string := "Aucun device mixer détecté sur cette machine.";
   err_mixer_open  : constant string := "Impossible d'ouvrir un mixer";
   -- erreur pas de microphone
   err_micro_title : constant string := "Pas de microphone !";
   err_micro_txt   : constant string := "Aucun microphone n'a été détecté sur cette machine !" & new_line
                                       & "Canta ne peut pas fonctionner sans microphone." & new_line
                                       & new_line
                                       & "Certaines cartes son exigent que le microphone soit connecté "
                                       & "sur la carte avant le lancement du logiciel.";

   -- erreur shell
   err_lance_aide  : constant string := "Il n'a pas été possible de lancer l'aide.";
   err_lance_web   : constant string := "Il n'a pas été possible de lancer le navigateur.";

   -- ouverture fichier midi
   err_fnf_txt     : constant string := "Fichier non trouvé ";
   err_midi_txt    : constant string := "Ce fichier n'est pas un fichier MIDI valide ";
   err_chant_txt   : constant string := "Ce fichier MIDI n'est pas utilisable dans cette application.";

   -- création fichier wav
   err_cre_wav     : constant string := "Impossible de créer le fichier !" & new_line & new_line
                                         & "Verifiez s'il n'est pas utilisé pas une autre application.";
   -- skins
   err_skin_not_found : constant String := "Impossible d'ouvrir le fichier de skin:" & new_line;
   err_skin_syntax  : constant String := "Le fichier de skin contient une erreur à la ligne";
   err_bitmap_inval : constant String := "Ce fichier n'est pas un bitmap valide" & new_line;
   err_bitmap_taille : constant String := "Taille de header invalide" & new_line;
   err_bitmap_sens : constant String := "Image top-down, invalide" & new_line;
   err_bitmap_plan : constant String := "Nombre de plans incorrect" & new_line;
   err_bitmap_32bits : constant string := "Ce bitmap n'est pas en 32 bits" & new_line;
   err_bitmap_compres : constant string := "Bitmap compressées, invalide" & new_line;
   err_bitmap_dens_h : constant String := "Densité horizontale /= 72 dpi" & new_line;
   err_bitmap_dens_v : constant string := "Densité verticale /= 72 dpi" & new_line;
   err_bitmap_true : constant String := "Ce bitmap n'est pas en true color" & new_line;
   err_bitmap_read : constant String := "Erreur dans la lecture du bitmap" & new_line;
   err_skin_missing : constant String := "Skin invalide, il manque le fichier pour" & new_line;
   err_bitmap_notfound : constant String := "Fichier bitmap manquant" & new_line;

   changement_skin : constant String := "Le changement de skin ne sera effectif que" & new_line
                                      & "lors du prochain démarrage de l'application.";
   -- informations
   Info_titre : constant String := "Information importante";


   -- dialogues
   dlg_param_title : constant string := "Sélection des paramètres";
   dlg_fs_title    : constant string := "Sélection de la fréquence";
   dlg_freq_txt    : constant string := "Fréquence";
   dlg_micro_txt   : constant string := "Microphone";
   dlg_ok_txt      : constant string := "OK";
   dlg_cancel_txt  : constant string := "Annuler";
   dlg_track_title : constant string := "Sélection de la piste mélodie";
   dlg_select_txt  : constant string := "Sélectionner";
   dlg_midi_title  : constant string := "Sélection du device MIDI";
   dlg_no_midi_txt : constant string := "Pas de device MIDI installé";
   dlg_device_txt  : constant String := "Device MIDI";
   dlg_piste_txt   : constant String := "  PISTES";
   dlg_instr_txt   : constant String := "INSTRUMENTS";
   -- Options
   dlg_auto_volume : constant String := "Volume automatique";
   dlg_change_instr: constant String := "Changer l'instrument";
   dlg_famille    :  constant String := "Famille d'instruments";
   dlg_instrument  : constant String := "Instrument de mélodie";
   dlg_skin        : constant String := "Interface (skin)";
   dlg_oui_sz      : constant String := "Oui" & ascii.nul;
   dlg_non_sz      : constant String := "Non" & ascii.nul;
   Option_dlg_title: constant String := "Options";
   -- Dialod No control
   dlg_nocontrol_title : constant String := "Volume microphone";
   dlg_novolum_txt : constant String := "Le programme n'a pas été capable d'identifier automatiquement un bouton de réglage du volume." & new_line
              & new_line
              & "Néanmoins, il est possible que ce réglage puisse être fait par l'application ""Contrôle de volume"" de Windows." & new_line
              & new_line
              & "Si vous souhaitez lancer cette application vous pouvez cliquer sur le bouton ci-dessous:";
   dlg_lancer_txt : constant String := "Lancer ""Contrôle de volume""";

   -- nom piste Midi
   Voix : constant String := "Voix";

   -- presse-papier
   clipboard_error : constant String := "Le presse-papier doit être fermé pour que la copie soit possible.";


end Intl;
