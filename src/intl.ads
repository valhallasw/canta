package Intl is

   langue : constant string := "fr";

   new_line : constant string := ( 1 => ascii.lf, 2 => ascii.cr );

   URL_Site : constant String := "http://www.chaumetsoftware.com/";

   -- ----------------------------------------------------------------------------------
   demo : constant string := "DEMO";

   demo_text  : constant string := "Cette version de d�monstration va s'arr�ter maintenant." & new_line
                                    & "La version compl�te n'est pas limit�e dans le temps." & new_line
                                    & "Cliquez sur le bouton 'Web' pour acheter la version compl�te.";

   attente_demo_text1 : constant string := "Vous devez attendre";
   attente_demo_text2 : constant string := " avant de pouvoir relancer la d�mo.";

   demo_finie : constant string := "D�monstration termin�e";

   -- -------------------------------------------------------------------------------------------

   -- Message d'erreur � l'execution d'une cl� invalide
   titre_invalide : constant String := "Cl� invalide !";

   suite_inval : constant string := "Veuillez vous connectez sur le site " & URL_Site
                                      & " pour obtenir une cl� valide.";
   cle_invalide : constant String := "La cl� d'autorisation n'est pas valide." & new_line
                                      & suite_inval ;

   key_not_found : constant String := "La cl� de validation n'a pas �t� trouv�e." & new_line
                                       & "Vous pouvez la t�l�charg�e grace au le lien situ� dans le mail qui vous a �t� envoy� lors de votre achat." & new_line
                                       & "Veuillez la copier dans le r�pertoire d'installation de Canta."
                                       & new_line & new_line & "Code de validation: ";

   -- ----------------------------------------------------------------------------------------------
   -- Dialog pour la cl� de validation

   -- bouton Copier pour la signature hard
   dlg_copier_txt  : constant String := "Copier";

   -- label signature
   dlg_sign_txt   : constant String := "Code de validation:";

   -- version Demo:
   dlg_demo_title : constant string := "Limitation de la version de d�monstration";

   dlg_demo_intro : constant string := "Cette version de d�monstration est limit�e en temps."
                                    & "Elle va s'arr�ter au bout de dix minutes de fonctionnement"
                                    & "et ne poura �tre relanc�e qu'apr�s dix minutes de pause."
                                    & "Par contre, toutes les fonctions sont pr�sentes et vous pouvez les tester librement." & new_line
                                    & new_line
                                    & "La version compl�te n'est pas limit�e dans le temps, "
                                    & "vous pouvez l'acheter en cliquant sur le bouton 'Web'." & new_line
                                    & "Pour cela vous aurez besoin du code de validation ci-dessous." & new_line
                                    & "Vous pouvez le copier dans le presse-papier en cliquant sur le bouton 'Copier'.";


   -- version Full:
   dlg_auto_title : constant string := "Programme non autoris�";

   dlg_instruct_txt : constant String :=  "La cl� d'autorisation n'est pas valide." & new_line
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
   Debut_player : constant String := "d�but:";
   Fin_player   : constant String := "fin:";
   Vol_melodie	: constant String := "M";
   Vol_accomp	: constant String := "A";
   Transpo	: constant String := "T:";
   Decalage	: constant String := "D:";


   -- nom des notes
   subtype string_4 is string(1..4);
   Nom_notes : constant array(0..11) of string_4 := (
   "  Do", " Do#","  R�", " R�#", "  Mi", "  Fa", " Fa#", " Sol", "Sol#", "  La", " La#", "  Si");

   -- messages d'erreur
   def_err_title   : constant string := "Erreur !";
   err_exception   : constant string := "Une erreur fatale s'est produite ! Veuillez consultez le fichier 'log.txt'.";

   err_init_txt    : constant string := "Erreur lors de l'initialisation du programme";
   err_win_txt     : constant string := "Erreur de cr�ation de la window ";
   err_class_txt   : constant string := "Erreur dans la cr�ation de la class ";
   err_class_title : constant string := "Erreur de cr�ation de classe";
   err_obj_txt     : constant string := "Erreur dans la cr�ation de l'objet Id=";
   -- erreur MIDI device
   err_play_title  : constant string := "Erreur du player MIDI";
   err_timer_txt   : constant string := "Le timer n'a pas pu �tre d�marr�";
   err_midi_title  : constant string := "Erreur de device MIDI";
   err_midi_dev_txt: constant string := "Aucun device MIDI identifi� sur cette machine";
   -- erreur mixer
   err_no_mixer    : constant string := "Aucun device mixer d�tect� sur cette machine.";
   err_mixer_open  : constant string := "Impossible d'ouvrir un mixer";
   -- erreur pas de microphone
   err_micro_title : constant string := "Pas de microphone !";
   err_micro_txt   : constant string := "Aucun microphone n'a �t� d�tect� sur cette machine !" & new_line
                                       & "Canta ne peut pas fonctionner sans microphone." & new_line
                                       & new_line
                                       & "Certaines cartes son exigent que le microphone soit connect� "
                                       & "sur la carte avant le lancement du logiciel.";

   -- erreur shell
   err_lance_aide  : constant string := "Il n'a pas �t� possible de lancer l'aide.";
   err_lance_web   : constant string := "Il n'a pas �t� possible de lancer le navigateur.";

   -- ouverture fichier midi
   err_fnf_txt     : constant string := "Fichier non trouv� ";
   err_midi_txt    : constant string := "Ce fichier n'est pas un fichier MIDI valide ";
   err_chant_txt   : constant string := "Ce fichier MIDI n'est pas utilisable dans cette application.";

   -- cr�ation fichier wav
   err_cre_wav     : constant string := "Impossible de cr�er le fichier !" & new_line & new_line
                                         & "Verifiez s'il n'est pas utilis� pas une autre application.";
   -- skins
   err_skin_not_found : constant String := "Impossible d'ouvrir le fichier de skin:" & new_line;
   err_skin_syntax  : constant String := "Le fichier de skin contient une erreur � la ligne";
   err_bitmap_inval : constant String := "Ce fichier n'est pas un bitmap valide" & new_line;
   err_bitmap_taille : constant String := "Taille de header invalide" & new_line;
   err_bitmap_sens : constant String := "Image top-down, invalide" & new_line;
   err_bitmap_plan : constant String := "Nombre de plans incorrect" & new_line;
   err_bitmap_32bits : constant string := "Ce bitmap n'est pas en 32 bits" & new_line;
   err_bitmap_compres : constant string := "Bitmap compress�es, invalide" & new_line;
   err_bitmap_dens_h : constant String := "Densit� horizontale /= 72 dpi" & new_line;
   err_bitmap_dens_v : constant string := "Densit� verticale /= 72 dpi" & new_line;
   err_bitmap_true : constant String := "Ce bitmap n'est pas en true color" & new_line;
   err_bitmap_read : constant String := "Erreur dans la lecture du bitmap" & new_line;
   err_skin_missing : constant String := "Skin invalide, il manque le fichier pour" & new_line;
   err_bitmap_notfound : constant String := "Fichier bitmap manquant" & new_line;

   changement_skin : constant String := "Le changement de skin ne sera effectif que" & new_line
                                      & "lors du prochain d�marrage de l'application.";
   -- informations
   Info_titre : constant String := "Information importante";


   -- dialogues
   dlg_param_title : constant string := "S�lection des param�tres";
   dlg_fs_title    : constant string := "S�lection de la fr�quence";
   dlg_freq_txt    : constant string := "Fr�quence";
   dlg_micro_txt   : constant string := "Microphone";
   dlg_ok_txt      : constant string := "OK";
   dlg_cancel_txt  : constant string := "Annuler";
   dlg_track_title : constant string := "S�lection de la piste m�lodie";
   dlg_select_txt  : constant string := "S�lectionner";
   dlg_midi_title  : constant string := "S�lection du device MIDI";
   dlg_no_midi_txt : constant string := "Pas de device MIDI install�";
   dlg_device_txt  : constant String := "Device MIDI";
   dlg_piste_txt   : constant String := "  PISTES";
   dlg_instr_txt   : constant String := "INSTRUMENTS";
   -- Options
   dlg_auto_volume : constant String := "Volume automatique";
   dlg_change_instr: constant String := "Changer l'instrument";
   dlg_famille    :  constant String := "Famille d'instruments";
   dlg_instrument  : constant String := "Instrument de m�lodie";
   dlg_skin        : constant String := "Interface (skin)";
   dlg_oui_sz      : constant String := "Oui" & ascii.nul;
   dlg_non_sz      : constant String := "Non" & ascii.nul;
   Option_dlg_title: constant String := "Options";
   -- Dialod No control
   dlg_nocontrol_title : constant String := "Volume microphone";
   dlg_novolum_txt : constant String := "Le programme n'a pas �t� capable d'identifier automatiquement un bouton de r�glage du volume." & new_line
              & new_line
              & "N�anmoins, il est possible que ce r�glage puisse �tre fait par l'application ""Contr�le de volume"" de Windows." & new_line
              & new_line
              & "Si vous souhaitez lancer cette application vous pouvez cliquer sur le bouton ci-dessous:";
   dlg_lancer_txt : constant String := "Lancer ""Contr�le de volume""";

   -- nom piste Midi
   Voix : constant String := "Voix";

   -- presse-papier
   clipboard_error : constant String := "Le presse-papier doit �tre ferm� pour que la copie soit possible.";


end Intl;
