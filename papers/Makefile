SUFFIXES = .html $(SUFFIXES) 
DIR=/Users/bullo/lib/bib

FBalias=$(DIR)/alias.bib
FBbibfile=$(DIR)/FB.bib
FBpreface=preface.bib

FBtmp1=tmp1.bib
FBtmp2=tmp2.bib

default: html

pub: html

html: Makefile $(FBbibfile)
	bibtool "--delete.field{funding}" "--delete.field{abstract}" "--delete.field{url}" "--delete.field{nopdf}" "--delete.field{nojournal}" "--delete.field{fbnote}" "--delete.field{annote}" "--delete.field{nourl}" -i $(FBbibfile) -o $(FBtmp2)
	cat $(FBpreface) $(FBalias) $(FBtmp2) > $(FBtmp1)
	sed s/D.....rfler/Dorfler/  $(FBtmp1) > $(FBtmp2)
	sed s/B.....rger/Burger/  $(FBtmp2) > $(FBtmp1)
	sed s/Allg.....wer/Allgower/  $(FBtmp1) > $(FBtmp2)
	sed s/Canc..un,.M..exico/"Cancun, Mexico"/  $(FBtmp2) > $(FBtmp1)
	/Users/bullo/bin/change_string -- "textendash" "--" $(FBtmp1)
	rm -f $(FBtmp1).bak
	/Users/bullo/bin/change_string "\\\!" "" $(FBtmp1)
	rm -f $(FBtmp1).bak
	/Users/bullo/bin/change_string "textup" "  " $(FBtmp1)
	rm -f $(FBtmp1).bak
	/Users/bullo/bin/change_string "texttt" "  " $(FBtmp1)
	rm -f $(FBtmp1).bak
	/Users/bullo/bin/change_string "{\\\\'e}" "e" $(FBtmp1)
	rm -f $(FBtmp1).bak
	/Users/bullo/bin/change_string "{\\\\'\\\\i}" "i" $(FBtmp1)
	rm -f $(FBtmp1).bak
	/Users/bullo/bin/bibtex2html-1.02 -html-links -peon -sort cat -force -icons $(FBtmp1)
	rm -f $(FBtmp2) $(FBtmp1) *.bak
	rm -f *.bak */*.bak
	change_string "bgcolor=\"#669999\""  "" *.html */*.html; rm -f *.bak */*.bak
	change_string "color=\"#FFFFFF\""    "" *.html */*.html; rm -f *.bak */*.bak
	change_string "bgcolor=\"#badfe1\""  "bgcolor=\"#eeeeee\"" *.html */*.html; rm -f *.bak */*.bak
	change_string "face=\"times\""       "" *.html */*.html; rm -f *.bak */*.bak
	change_string "<body bg link=\"blue\" alink=\"blue\" vlink=\"blue\">" "" *.html */*.html; rm -f *.bak  */*.bak
	change_string "not working <A HREF=\"../index.html\"><b> BACK TO INDEX </b></A>" "" *.html */*.html; rm -f *.bak */*.bak
	change_string "<br><br>" "<br>" *.html */*.html; rm -f *.bak */*.bak
	change_string "<META name=\"GENERATOR\" content=\"bibtex2html 1.01\">" "<link rel=\"stylesheet\" href=\"https://fbullo.github.io/css/mysite-small.css\" type=\"text/css\" />"	*.html */*.html; rm -f *.bak */*.bak
	change_string "cellspacing=\"2\""     "" *.html */*.html; rm -f *.bak */*.bak
	change_string "font size=2"  "font size=1" *.html */*.html; rm -f *.bak */*.bak
	change_string "font size=4"  "font size=2" *.html */*.html; rm -f *.bak */*.bak
	change_string "FONT SIZE=6"  "font size=5" *.html */*.html; rm -f *.bak */*.bak
# accessibility: CSS instead of the align and valign attributes of the index tables
	change_string "<table align=\"center\"" "<table style=\"margin-left: auto; margin-right: auto\"" *.html */*.html; rm -f *.bak */*.bak
	change_string "<tr align=\"left\" valign=\"top\">" "<tr style=\"text-align: left; vertical-align: top\">" *.html */*.html; rm -f *.bak */*.bak
	change_string "<tr align=\"center\">" "<tr style=\"text-align: center\">" *.html */*.html; rm -f *.bak */*.bak
# stage new pages (new co-authors, keywords, years): "git commit -a" commits only
# files git already tracks, so new pages were never published (404 until 2026-10-15)
	if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then git add -A -- . ':!.DS_Store' ':!**/.DS_Store'; fi

new:
	change_string "<br>\n\n<br>"  "" *.html */*.html; rm -f *.bak */*.bak

oldbib:
	bibtool "--delete.field{funding}" -i $(FBbibfile) -o $(DIR)/fbullo-bibliography.bib

oldold:
	/Users/bullo/bin/change_string "<br>Author: bullo." ""  *.html */*.html
	clean -r

v:
	open http://fbullo.github.io/papers
