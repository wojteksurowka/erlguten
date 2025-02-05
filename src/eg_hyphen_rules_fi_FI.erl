%% @private
-module(eg_hyphen_rules_fi_FI).
%% autogenerated do not edit
%%[{Char,Val}] = after char Char Val is inserted
-export([hyphens/1, exception/1]).

exception(_) -> no.

hyphens("öö" ++ _)->[{1,2}];
hyphens("öä" ++ _)->[{1,2}];
hyphens("öy" ++ _)->[{1,2}];
hyphens("öu" ++ _)->[{2,2},{1,1}];
hyphens("öo" ++ _)->[{2,2},{1,3}];
hyphens("öe" ++ _)->[{2,2},{1,1}];
hyphens("öa" ++ _)->[{2,2},{1,3}];
hyphens("äö" ++ _)->[{1,2}];
hyphens("ääy" ++ _)->[{2,3}];
hyphens("ääi" ++ _)->[{2,1}];
hyphens("ääe" ++ _)->[{2,1}];
hyphens("ää" ++ _)->[{1,2}];
hyphens("äy" ++ _)->[{1,2}];
hyphens("äu" ++ _)->[{2,2},{1,1}];
hyphens("äo" ++ _)->[{2,2},{1,3}];
hyphens("äa" ++ _)->[{2,2},{1,3}];
hyphens("yää" ++ _)->[{1,1}];
hyphens("yu" ++ _)->[{2,2},{1,1}];
hyphens("yo" ++ _)->[{2,2},{1,1}];
hyphens("yliop" ++ _)->[{4,2},{3,1}];
hyphens("yei" ++ _)->[{1,1}];
hyphens("ya" ++ _)->[{2,2},{1,1}];
hyphens("vö" ++ _)->[{0,1}];
hyphens("vä" ++ _)->[{0,1}];
hyphens("vy" ++ _)->[{0,1}];
hyphens("vu" ++ _)->[{0,1}];
hyphens("vo" ++ _)->[{0,1}];
hyphens("vi" ++ _)->[{0,1}];
hyphens("ve" ++ _)->[{0,1}];
hyphens("va" ++ _)->[{0,1}];
hyphens("uö" ++ _)->[{2,2},{1,1}];
hyphens("uä" ++ _)->[{2,2},{1,1}];
hyphens("uy" ++ _)->[{2,2},{1,1}];
hyphens("uuo" ++ _)->[{3,2},{2,1}];
hyphens("uui" ++ _)->[{3,2},{2,1}];
hyphens("uue" ++ _)->[{3,2},{2,1}];
hyphens("uua" ++ _)->[{3,2},{2,1}];
hyphens("us" ++ _)->[{1,2}];
hyphens("uou" ++ _)->[{2,1}];
hyphens("uoa" ++ _)->[{2,1}];
hyphens("ulos" ++ _)->[{4,1},{3,2}];
hyphens("uie" ++ _)->[{2,1}];
hyphens("uee" ++ _)->[{1,1}];
hyphens("uea" ++ _)->[{2,1}];
hyphens("uaa" ++ _)->[{1,1}];
hyphens("tö" ++ _)->[{0,1}];
hyphens("tä" ++ _)->[{0,1}];
hyphens("ty" ++ _)->[{0,1}];
hyphens("tu" ++ _)->[{0,1}];
hyphens("to" ++ _)->[{0,1}];
hyphens("ti" ++ _)->[{0,1}];
hyphens("te" ++ _)->[{0,1}];
hyphens("ta" ++ _)->[{0,1}];
hyphens("sö" ++ _)->[{0,1}];
hyphens("sä" ++ _)->[{0,1}];
hyphens("syrit" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("syhti" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("sy" ++ _)->[{0,1}];
hyphens("su" ++ _)->[{0,1}];
hyphens("str" ++ _)->[{2,2},{0,1}];
hyphens("spli" ++ _)->[{2,2},{0,1}];
hyphens("sosa" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("sopist" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("sopisk" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("sohje" ++ _)->[{1,1},{0,2}];
hyphens("so" ++ _)->[{0,1}];
hyphens("sidea. sidean" ++ _)->[{9,2},{8,1},{7,2},{2,2},{1,1},{0,2}];
hyphens("si" ++ _)->[{0,1}];
hyphens("sesity" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("se" ++ _)->[{0,1}];
hyphens("sch tsh" ++ _)->[{6,2},{2,2}];
hyphens("sasia" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("sase" ++ _)->[{1,1},{0,2}];
hyphens("sapu" ++ _)->[{1,1},{0,2}];
hyphens("saloi" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("salen" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("sajo" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("sajatu" ++ _)->[{1,1},{0,2}];
hyphens("sa" ++ _)->[{0,1}];
hyphens("rö" ++ _)->[{0,1}];
hyphens("rä" ++ _)->[{0,1}];
hyphens("ry" ++ _)->[{0,1}];
hyphens("ru" ++ _)->[{0,1}];
hyphens("rtaus" ++ _)->[{5,1}];
hyphens("ro" ++ _)->[{0,1}];
hyphens("ri" ++ _)->[{0,1}];
hyphens("re" ++ _)->[{0,1}];
hyphens("ras las" ++ _)->[{5,2},{1,2}];
hyphens("ra" ++ _)->[{0,1}];
hyphens("qv qvi" ++ _)->[{4,2},{3,1},{1,2}];
hyphens("pö" ++ _)->[{0,1}];
hyphens("pä" ++ _)->[{0,1}];
hyphens("py" ++ _)->[{0,1}];
hyphens("pu" ++ _)->[{0,1}];
hyphens("pr pro" ++ _)->[{4,2},{3,1},{1,2}];
hyphens("po" ++ _)->[{0,1}];
hyphens("pl" ++ _)->[{1,2}];
hyphens("pi" ++ _)->[{0,1}];
hyphens("perus" ++ _)->[{5,1}];
hyphens("pe" ++ _)->[{0,1}];
hyphens("pa" ++ _)->[{0,1}];
hyphens("oö" ++ _)->[{1,1}];
hyphens("oä" ++ _)->[{1,1}];
hyphens("oy" ++ _)->[{1,1}];
hyphens("ouu" ++ _)->[{1,1}];
hyphens("ouo" ++ _)->[{2,1}];
hyphens("oui" ++ _)->[{1,1}];
hyphens("oue" ++ _)->[{2,1}];
hyphens("oiu" ++ _)->[{2,1}];
hyphens("oio" ++ _)->[{2,1}];
hyphens("oie" ++ _)->[{2,1}];
hyphens("oia" ++ _)->[{2,1}];
hyphens("oaa" ++ _)->[{1,1}];
hyphens("nö" ++ _)->[{0,1}];
hyphens("nä" ++ _)->[{0,1}];
hyphens("nylit" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("ny" ++ _)->[{0,1}];
hyphens("nu" ++ _)->[{0,1}];
hyphens("notto noton" ++ _)->[{7,1},{6,2},{1,1},{0,2}];
hyphens("nosa" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("nopist" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("nopet" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("nomai" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("no" ++ _)->[{0,1}];
hyphens("ni" ++ _)->[{0,1}];
hyphens("nedus" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("ne" ++ _)->[{0,1}];
hyphens("nanto nanno" ++ _)->[{7,1},{6,2},{1,1},{0,2}];
hyphens("nalen" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("najo" ++ _)->[{2,2},{1,1},{0,2}];
hyphens("najan naika" ++ _)->[{7,1},{6,2},{2,2},{1,1},{0,2}];
hyphens("na" ++ _)->[{0,1}];
hyphens("mö" ++ _)->[{0,1}];
hyphens("mä" ++ _)->[{0,1}];
hyphens("my" ++ _)->[{0,1}];
hyphens("mu" ++ _)->[{0,1}];
hyphens("mo" ++ _)->[{0,1}];
hyphens("mi" ++ _)->[{0,1}];
hyphens("me" ++ _)->[{0,1}];
hyphens("ma" ++ _)->[{0,1}];
hyphens("lö" ++ _)->[{0,1}];
hyphens("lä" ++ _)->[{0,1}];
hyphens("ly" ++ _)->[{0,1}];
hyphens("lu" ++ _)->[{0,1}];
hyphens("lo" ++ _)->[{0,1}];
hyphens("li" ++ _)->[{0,1}];
hyphens("le" ++ _)->[{0,1}];
hyphens("la" ++ _)->[{0,1}];
hyphens("kö" ++ _)->[{0,1}];
hyphens("kä" ++ _)->[{0,1}];
hyphens("ky" ++ _)->[{0,1}];
hyphens("kv kva" ++ _)->[{4,2},{3,1},{1,2},{0,1}];
hyphens("ku" ++ _)->[{0,1}];
hyphens("kra kre kri" ++ _)->[{9,2},{8,1},{5,2},{4,1},{1,2},{0,1}];
hyphens("ko" ++ _)->[{0,1}];
hyphens("kl" ++ _)->[{1,2}];
hyphens("ki" ++ _)->[{0,1}];
hyphens("keus" ++ _)->[{4,1}];
hyphens("ke" ++ _)->[{0,1}];
hyphens("ka" ++ _)->[{0,1}];
hyphens("jö" ++ _)->[{0,1}];
hyphens("jä" ++ _)->[{0,1}];
hyphens("jy" ++ _)->[{0,1}];
hyphens("ju" ++ _)->[{0,1}];
hyphens("jo" ++ _)->[{0,1}];
hyphens("ji" ++ _)->[{0,1}];
hyphens("je" ++ _)->[{0,1}];
hyphens("ja" ++ _)->[{0,1}];
hyphens("iöö" ++ _)->[{1,1}];
hyphens("iää" ++ _)->[{1,1}];
hyphens("iuu" ++ _)->[{1,1}];
hyphens("iuo" ++ _)->[{2,1}];
hyphens("iue" ++ _)->[{2,1}];
hyphens("iua" ++ _)->[{2,1}];
hyphens("ioe" ++ _)->[{3,2},{2,1}];
hyphens("ioa" ++ _)->[{3,2},{2,1}];
hyphens("iio" ++ _)->[{3,2},{2,1}];
hyphens("iie" ++ _)->[{3,2},{2,1}];
hyphens("iia" ++ _)->[{3,2},{2,1}];
hyphens("iey" ++ _)->[{2,1}];
hyphens("ieo" ++ _)->[{2,1}];
hyphens("iea" ++ _)->[{2,1}];
hyphens("iau" ++ _)->[{1,1}];
hyphens("iaa" ++ _)->[{1,1}];
hyphens("hö" ++ _)->[{0,1}];
hyphens("hä" ++ _)->[{0,1}];
hyphens("hy" ++ _)->[{0,1}];
hyphens("hu" ++ _)->[{0,1}];
hyphens("ho" ++ _)->[{0,1}];
hyphens("hi" ++ _)->[{0,1}];
hyphens("he" ++ _)->[{0,1}];
hyphens("ha" ++ _)->[{0,1}];
hyphens("gö" ++ _)->[{0,1}];
hyphens("gä" ++ _)->[{0,1}];
hyphens("gy" ++ _)->[{0,1}];
hyphens("gu" ++ _)->[{0,1}];
hyphens("gr gra" ++ _)->[{4,2},{3,1},{1,2}];
hyphens("go" ++ _)->[{0,1}];
hyphens("gl glo" ++ _)->[{4,2},{3,1},{1,2}];
hyphens("gi" ++ _)->[{0,1}];
hyphens("ge" ++ _)->[{0,1}];
hyphens("ga" ++ _)->[{0,1}];
hyphens("fy" ++ _)->[{0,1}];
hyphens("fu" ++ _)->[{0,1}];
hyphens("fr fra fre" ++ _)->[{8,2},{7,1},{4,2},{3,1},{1,2}];
hyphens("fo" ++ _)->[{0,1}];
hyphens("fl fla" ++ _)->[{4,2},{3,1},{1,2}];
hyphens("fi" ++ _)->[{0,1}];
hyphens("fe" ++ _)->[{0,1}];
hyphens("fa" ++ _)->[{0,1}];
hyphens("eö" ++ _)->[{2,2},{1,1}];
hyphens("eää" ++ _)->[{1,1}];
hyphens("euu" ++ _)->[{1,1}];
hyphens("eua" ++ _)->[{2,1}];
hyphens("eey" ++ _)->[{3,2},{2,1}];
hyphens("eeu" ++ _)->[{3,2},{2,1}];
hyphens("eei" ++ _)->[{3,2},{2,1}];
hyphens("eea" ++ _)->[{3,2},{2,1}];
hyphens("eai" ++ _)->[{1,1}];
hyphens("eaa" ++ _)->[{1,1}];
hyphens("dö" ++ _)->[{0,1}];
hyphens("dä" ++ _)->[{0,1}];
hyphens("dy" ++ _)->[{0,1}];
hyphens("du" ++ _)->[{0,1}];
hyphens("dr dra" ++ _)->[{4,2},{3,1},{1,2}];
hyphens("do" ++ _)->[{0,1}];
hyphens("di" ++ _)->[{0,1}];
hyphens("de" ++ _)->[{0,1}];
hyphens("da" ++ _)->[{0,1}];
hyphens("cl" ++ _)->[{1,2}];
hyphens("chr" ++ _)->[{2,2}];
hyphens("by" ++ _)->[{0,1}];
hyphens("bu" ++ _)->[{0,1}];
hyphens("br bri bro bru" ++ _)->[{12,2},{11,1},{8,2},{7,1},{4,2},{3,1},{1,2}];
hyphens("bo" ++ _)->[{0,1}];
hyphens("bl blo bibli" ++ _)->[{10,3},{4,2},{3,1},{1,2}];
hyphens("bi" ++ _)->[{0,1}];
hyphens("be" ++ _)->[{0,1}];
hyphens("ba" ++ _)->[{0,1}];
hyphens("aö" ++ _)->[{1,1}];
hyphens("aä" ++ _)->[{1,1}];
hyphens("auu" ++ _)->[{1,1}];
hyphens("aue" ++ _)->[{2,1}];
hyphens("aua" ++ _)->[{2,1}];
hyphens("asioi" ++ _)->[{1,2},{0,1}];
hyphens("asiat" ++ _)->[{1,2},{0,1}];
hyphens("asian" ++ _)->[{1,2},{0,1}];
hyphens("asiakas" ++ _)->[{7,1},{6,2},{1,2},{0,1}];
hyphens("aoi" ++ _)->[{1,1}];
hyphens("alous" ++ _)->[{5,1}];
hyphens("alkeis" ++ _)->[{6,1},{5,2}];
hyphens("aliav" ++ _)->[{4,2},{3,1}];
hyphens("aiu" ++ _)->[{2,1}];
hyphens("aio" ++ _)->[{2,1}];
hyphens("aie" ++ _)->[{2,1}];
hyphens("aia" ++ _)->[{2,1}];
hyphens("aei" ++ _)->[{1,1}];
hyphens("aau" ++ _)->[{3,2},{2,1}];
hyphens("aao" ++ _)->[{3,2},{2,1}];
hyphens("aai" ++ _)->[{3,2},{2,1}];
hyphens("aae" ++ _)->[{3,2},{2,1}];
hyphens(".ä" ++ _)->[{2,2}];
hyphens(".ydin" ++ _)->[{5,1},{4,2}];
hyphens(".suura" ++ _)->[{6,2},{5,1},{4,2}];
hyphens(_) -> [].
