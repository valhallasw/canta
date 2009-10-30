with Common_types;

package general_midi is

   drum_channel : constant := 9;

   subtype instrument_range_type is integer range 0..127;

   -- limite instruments mélodiques
   max_melodique : constant integer := 111;

Instrument_name : constant array(instrument_range_type) of Common_types.String_pt :=
(
-- PIANO
 0 => new string'("Acoustic Grand"),
 1 => new string'("Bright Acoustic"),
 2 => new string'("Electric Grand"),
 3 => new string'("Honky-Tonk"),
 4 => new string'("Electric Piano 1"),
 5 => new string'("Electric Piano 2"),
 6 => new string'("Harpsichord"),
 7 => new string'("Clavinet"),
--  CHROMATIC PERCUSSION
 8 => new string'("Celesta"),
 9 => new string'("Glockenspiel"),
10 => new string'("Music Box"),
11 => new string'("Vibraphone"),
12 => new string'("Marimba"),
13 => new string'("Xylophone"),
14 => new string'("Tubular Bells"),
15 => new string'("Dulcimer"),
--  ORGAN
16 => new string'("Drawbar Organ"),
17 => new string'("Percussive Organ"),
18 => new string'("Rock Organ"),
19 => new string'("Church Organ"),
20 => new string'("Reed Organ"),
21 => new string'("Accordion"),
22 => new string'("Harmonica"),
23 => new string'("Tango Accordion"),
--  GUITAR
24 => new string'("Nylon String Guitar"),
25 => new string'("Steel String Guitar"),
26 => new string'("Electric Jazz Guitar"),
27 => new string'("Electric Clean Guitar"),
28 => new string'("Electric Muted Guitar"),
29 => new string'("Overdriven Guitar"),
30 => new string'("Distortion Guitar"),
31 => new string'("Guitar Harmonics"),
--BASS
32 => new string'("Acoustic Bass"),
33 => new string'("Electric Bass(finger)"),
34 => new string'("Electric Bass(pick)"),
35 => new string'("Fretless Bass"),
36 => new string'("Slap Bass 1"),
37 => new string'("Slap Bass 2"),
38 => new string'("Synth Bass 1"),
39 => new string'("Synth Bass 2"),
--SOLO STRINGS
40 => new string'("Violin"),
41 => new string'("Viola"),
42 => new string'("Cello"),
43 => new string'("Contrabass"),
44 => new string'("Tremolo Strings"),
45 => new string'("Pizzicato Strings"),
46 => new string'("Orchestral Strings"),
47 => new string'("Timpani"),
--ENSEMBLE
48 => new string'("String Ensemble 1"),
49 => new string'("String Ensemble 2"),
50 => new string'("SynthStrings 1"),
51 => new string'("SynthStrings 2"),
52 => new string'("Choir Aahs"),
53 => new string'("Voice Oohs"),
54 => new string'("Synth Voice"),
55 => new string'("Orchestra Hit"),
--BRASS
56 => new string'("Trumpet"),
57 => new string'("Trombone"),
58 => new string'("Tuba"),
59 => new string'("Muted Trumpet"),
60 => new string'("French Horn"),
61 => new string'("Brass Section"),
62 => new string'("SynthBrass 1"),
63 => new string'("SynthBrass 2"),
--REED
64 => new string'("Soprano Sax"),
65 => new string'("Alto Sax"),
66 => new string'("Tenor Sax"),
67 => new string'("Baritone Sax"),
68 => new string'("Oboe"),
69 => new string'("English Horn"),
70 => new string'("Bassoon"),
71 => new string'("Clarinet"),
--PIPE
72 => new string'("Piccolo"),
73 => new string'("Flute"),
74 => new string'("Recorder"),
75 => new string'("Pan Flute"),
76 => new string'("Blown Bottle"),
77 => new string'("Skakuhachi"),
78 => new string'("Whistle"),
79 => new string'("Ocarina"),
--SYNTH LEAD
80 => new string'("Lead 1 (square)"),
81 => new string'("Lead 2 (sawtooth)"),
82 => new string'("Lead 3 (calliope)"),
83 => new string'("Lead 4 (chiff)"),
84 => new string'("Lead 5 (charang)"),
85 => new string'("Lead 6 (voice)"),
86 => new string'("Lead 7 (fifths)"),
87 => new string'("Lead 8 (bass+lead)"),
--SYNTH PAD
88 => new string'("Pad 1 (new age)"),
89 => new string'("Pad 2 (warm)"),
90 => new string'("Pad 3 (polysynth)"),
91 => new string'("Pad 4 (choir)"),
92 => new string'("Pad 5 (bowed)"),
93 => new string'("Pad 6 (metallic)"),
94 => new string'("Pad 7 (halo)"),
95 => new string'("Pad 8 (sweep)"),
-- SYNTH EFFECTS
 96 => new string'("FX 1 (rain)"),
 97 => new string'("FX 2 (soundtrack)"),
 98 => new string'("FX 3 (crystal)"),
 99 => new string'("FX 4 (atmosphere)"),
100 => new string'("FX 5 (brightness)"),
101 => new string'("FX 6 (goblins)"),
102 => new string'("FX 7 (echoes)"),
103 => new string'("FX 8 (sci-fi)"),
--ETHNIC
104 => new string'("Sitar"),
105 => new string'("Banjo"),
106 => new string'("Shamisen"),
107 => new string'("Koto"),
108 => new string'("Kalimba"),
109 => new string'("Bagpipe"),
110 => new string'("Fiddle"),
111 => new string'("Shanai"),
-- PERCUSSIVE
112 => new string'("Tinkle Bell"),
113 => new string'("Agogo"),
114 => new string'("Steel Drums"),
115 => new string'("Woodblock"),
116 => new string'("Taiko Drum"),
117 => new string'("Melodic Tom"),
118 => new string'("Synth Drum"),
119 => new string'("Reverse Cymbal"),
-- SOUND EFFECTS
120 => new string'("Guitar Fret Noise"),
121 => new string'("Breath Noise"),
122 => new string'("Seashore"),
123 => new string'("Bird Tweet"),
124 => new string'("Telephone Ring"),
125 => new string'("Helicopter"),
126 => new string'("Applause"),
127 => new string'("Gunshot")
);

   -- familles d'instruments
   families : constant array(0..15) of Common_types.String_pt :=
   		(  0 => new string'("Piano"),
                   1 => new string'("Chromatic percussion"),
                   2 => new string'("Organ"),
                   3 => new string'("Guitar"),
                   4 => new string'("Bass"),
                   5 => new string'("Solo string"),
                   6 => new string'("Ensemble"),
                   7 => new string'("Brass"),
                   8 => new string'("Reed"),
                   9 => new string'("Pipe"),
                  10 => new string'("Synth lead"),
                  11 => new string'("Synth pad"),
                  12 => new string'("Synth effects"),
                  13 => new string'("Ethnic"),
                  14 => new string'("Percusive"),
                  15 => new string'("Sound effects") );

   -- partie de la batterie
   grosse_caisse 	: constant integer := 41;
   basse_drum	 	: constant integer := 35;
   snare 		: constant integer := 38;

   subtype drum_range is integer range 35..81;

drum_sound : constant array(drum_range) of  Common_types.String_pt :=
(
35 => new string'("Acoustic Bass Drum"),
36 => new string'("Bass Drum 1"),
37 => new string'("Side Stick"),
38 => new string'("Acoustic Snare"),
39 => new string'("Hand Clap"),
40 => new string'("Electric Snare"),
41 => new string'("Low Floor Tom"),
42 => new string'("Closed Hi-Hat"),
43 => new string'("High Floor Tom"),
44 => new string'("Pedal Hi-Hat"),
45 => new string'("Low Tom"),
46 => new string'("Open Hi-Hat"),
47 => new string'("Low-Mid Tom"),
48 => new string'("Hi-Mid Tom"),
49 => new string'("Crash Cymbal 1"),
50 => new string'("High Tom"),
51 => new string'("Ride Cymbal 1"),
52 => new string'("Chinese Cymbal"),
53 => new string'("Ride Bell"),
54 => new string'("Tambourine"),
55 => new string'("Splash Cymbal"),
56 => new string'("Cowbell"),
57 => new string'("Crash Cymbal 2"),
58 => new string'("Vibraslap"),
59 => new string'("Ride Cymbal 2"),
60 => new string'("Hi Bongo"),
61 => new string'("Low Bongo"),
62 => new string'("Mute Hi Conga"),
63 => new string'("Open Hi Conga"),
64 => new string'("Low Conga"),
65 => new string'("High Timbale"),
66 => new string'("Low Timbale"),
67 => new string'("High Agogo"),
68 => new string'("Low Agogo"),
69 => new string'("Cabasa"),
70 => new string'("Maracas"),
71 => new string'("Short Whistle"),
72 => new string'("Long Whistle"),
73 => new string'("Short Guiro"),
74 => new string'("Long Guiro"),
75 => new string'("Claves"),
76 => new string'("Hi Wood Block"),
77 => new string'("Low Wood Block"),
78 => new string'("Mute Cuica"),
79 => new string'("Open Cuica"),
80 => new string'("Mute Triangle"),
81 => new string'("Open Triangle")
);


end general_midi;
