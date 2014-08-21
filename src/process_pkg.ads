with Common_types;

package Process_pkg is

   -- constantes et tableau pour le spectre
   prem_bin : constant := 2;	-- bin pour 60Hz
   dern_bin : constant := 58;	-- bin pour ~1200 Hz
   num_bins : constant := dern_bin - prem_bin + 1;
   spectre : array(1..num_bins) of Long_Float;	-- tableau pour le spectre


   -- création de la class Data
   procedure Create_Data_Class;

   -- window qui va recevoir les notifications et lancer le traitement
   procedure Create_Data_Win;

   -- doit être appelé au ébut et à chaque changement de fréquence d'échantillonnage
   procedure Init_data;

   -- sauvegarde au format wave et Midi
   function Demarre_sauvegarde( root : string ) return boolean;

   -- termine la sauvegarde et ferme le fichier
   procedure Termine_sauvegarde;

   -- retourne le temps de la carte son en millisecondes
   function Card_clock return integer;

   -- entre ou sort du mode 'gel'
   procedure Toggle_freeze;
   procedure Start_freeze;
   procedure End_freeze;

end Process_pkg;
