%% @private
-module(eg_font_14).
-export([width/1, kern/2, fontName/0, firstChar/0,lastChar/0]).
-export([index/0,ascender/0,capHeight/0,descender/0,italicAngle/0]).
-export([xHeight/0, flags/0, type/0, stemV/0,fontBBox/0,widths/0]).
-export([encoding/0]).
fontName() -> "ZapfDingbats".
index() -> 14.
type() -> internal.
encoding() -> "FontSpecific".
firstChar() ->32.
lastChar() ->254.
ascender() ->0.
capHeight() ->0.
descender() ->0.
italicAngle() ->0.
xHeight() ->0.
flags() ->4.
stemV() ->0.
fontBBox() ->{-1,-143,981,820}.
widths() ->[278,974,961,974,980,719,789,790,791,690,960,939,549,855,911,933,911,945,974,
 755,846,762,761,571,677,763,760,759,754,494,552,537,577,692,786,788,788,790,
 793,794,816,823,789,841,823,833,816,831,923,744,723,749,790,792,695,776,768,
 792,759,707,708,682,701,826,815,789,789,707,687,696,689,786,787,713,791,785,
 791,873,761,762,762,759,759,892,892,788,784,438,138,277,415,392,392,668,668,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,732,544,
 544,910,667,760,760,776,595,694,626,788,788,788,788,788,788,788,788,788,788,
 788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,788,
 788,788,788,788,788,788,788,788,788,788,788,894,838,1016,458,748,924,748,918,
 927,928,928,834,873,828,924,924,917,930,931,463,883,836,836,867,867,696,696,
 874,0,874,760,946,771,865,771,888,967,888,831,873,927,970,918].
width(32)->278;
width(33)->974;
width(34)->961;
width(35)->974;
width(36)->980;
width(37)->719;
width(38)->789;
width(39)->790;
width(40)->791;
width(41)->690;
width(42)->960;
width(43)->939;
width(44)->549;
width(45)->855;
width(46)->911;
width(47)->933;
width(48)->911;
width(49)->945;
width(50)->974;
width(51)->755;
width(52)->846;
width(53)->762;
width(54)->761;
width(55)->571;
width(56)->677;
width(57)->763;
width(58)->760;
width(59)->759;
width(60)->754;
width(61)->494;
width(62)->552;
width(63)->537;
width(64)->577;
width(65)->692;
width(66)->786;
width(67)->788;
width(68)->788;
width(69)->790;
width(70)->793;
width(71)->794;
width(72)->816;
width(73)->823;
width(74)->789;
width(75)->841;
width(76)->823;
width(77)->833;
width(78)->816;
width(79)->831;
width(80)->923;
width(81)->744;
width(82)->723;
width(83)->749;
width(84)->790;
width(85)->792;
width(86)->695;
width(87)->776;
width(88)->768;
width(89)->792;
width(90)->759;
width(91)->707;
width(92)->708;
width(93)->682;
width(94)->701;
width(95)->826;
width(96)->815;
width(97)->789;
width(98)->789;
width(99)->707;
width(100)->687;
width(101)->696;
width(102)->689;
width(103)->786;
width(104)->787;
width(105)->713;
width(106)->791;
width(107)->785;
width(108)->791;
width(109)->873;
width(110)->761;
width(111)->762;
width(112)->762;
width(113)->759;
width(114)->759;
width(115)->892;
width(116)->892;
width(117)->788;
width(118)->784;
width(119)->438;
width(120)->138;
width(121)->277;
width(122)->415;
width(123)->392;
width(124)->392;
width(125)->668;
width(126)->668;
width(161)->732;
width(162)->544;
width(163)->544;
width(164)->910;
width(165)->667;
width(166)->760;
width(167)->760;
width(168)->776;
width(169)->595;
width(170)->694;
width(171)->626;
width(172)->788;
width(173)->788;
width(174)->788;
width(175)->788;
width(176)->788;
width(177)->788;
width(178)->788;
width(179)->788;
width(180)->788;
width(181)->788;
width(182)->788;
width(183)->788;
width(184)->788;
width(185)->788;
width(186)->788;
width(187)->788;
width(188)->788;
width(189)->788;
width(190)->788;
width(191)->788;
width(192)->788;
width(193)->788;
width(194)->788;
width(195)->788;
width(196)->788;
width(197)->788;
width(198)->788;
width(199)->788;
width(200)->788;
width(201)->788;
width(202)->788;
width(203)->788;
width(204)->788;
width(205)->788;
width(206)->788;
width(207)->788;
width(208)->788;
width(209)->788;
width(210)->788;
width(211)->788;
width(212)->894;
width(213)->838;
width(214)->1016;
width(215)->458;
width(216)->748;
width(217)->924;
width(218)->748;
width(219)->918;
width(220)->927;
width(221)->928;
width(222)->928;
width(223)->834;
width(224)->873;
width(225)->828;
width(226)->924;
width(227)->924;
width(228)->917;
width(229)->930;
width(230)->931;
width(231)->463;
width(232)->883;
width(233)->836;
width(234)->836;
width(235)->867;
width(236)->867;
width(237)->696;
width(238)->696;
width(239)->874;
width(241)->874;
width(242)->760;
width(243)->946;
width(244)->771;
width(245)->865;
width(246)->771;
width(247)->888;
width(248)->967;
width(249)->888;
width(250)->831;
width(251)->873;
width(252)->927;
width(253)->970;
width(254)->918;
width(_)->unknown.
kern(_,_) -> 0.
