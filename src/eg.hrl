
-record(env,	{pdf,             % PDF process
		 currentBox,      % name of currentBox
		 template,        % current template (Mopdule Name)
		 page,            % current page
		 dict,            % dictionary with keys{free,Page,Box}
				  %  {initialsed, Page, Template}
		 tagMap           % current TagMap (function of the box)
		 }).

-record(afm2, {baseFont,    % FontName
	       firstChar,   % first char in widths table
	       lastChar,    % last char in Widths table
	       widths,      % the widths table (0=undefined)
	       kernPairs,   % [{{C1,C2},K}]
	       ascender,    % num
	       capHeight,   % num
	       descender,   % num
	       flags,       % num
	       fontBBox,    % {int,int,int,int}
	       italicAngle, % num 
	       stemV,       % num
	       xHeight}).   % num
 
-record(info, {creator,creationDate, producer, author, title, subject,
	       keywords}).

-record(pdfContext, {
	  info,
	  fonts,
	  font_handler,
	  images=[], 
	  currentpage,
	  pages=[],
	  scripts=[],
	  mediabox,
	  procset={undefined,undefined}  %% { imageb,imagec }
	 }).


-record(image,{alias,width,height}).

-record(box,{x=10,              % X coordinate of top left hand corner of box
	     y=800,             % Y coordinate of top left hand corner of box
	     width=210,         % Width in points (72 point=1 inch)
	     leading=12,        % leading in points
	     lines=1,     % # lines in box
	     next=none,   % next= none | {DeltaPage, Tag} 
	     free=1,      % first free line in the box
	     grid=false,  % plot a grid
	     bg=default   % background color in box= default | {R,G,B}
	    }).

-record(tagMap, {name=default,        % Tag name
		 font="Times-Roman",  % Font name
		 color=default,       % default | {R,G,B}
		 size=11,             % font size
		 voff=0,              % vertical offset
		 break=true           % can do line breaks in blanks
		}).   


-record(face, {font, pointSize, vOffset, color, breakable}).







