# The MIT License
#
# Copyright (C) 2012 Universal Shell Programming Laboratory
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

NAME=		open-usp-tukubai

LOCALBASE?=	/opt/usp
BINDIR?=	${LOCALBASE}/bin
TUKDIR?=	${LOCALBASE}/share/${NAME}
DATDIR?=	${TUKDIR}/devel
MANDIR=		${DATDIR}/man
HTMDIR=		${DATDIR}/html
PDFDIR=		${DATDIR}/pdf
DOCDIR=		${DATDIR}/doc
PROFILEDIR=	/etc/profile.d

COMMANDS=	cgi-name check_attr_name check_need_name cjoin0 cjoin1 \
		cjoin2 comma count ctail delf divsen filehame getfirst \
		getlast gyo han join0 join1 join2 juni kasan keta keycut \
		loopj loopx map marume mdate mime-read mojihame nameread \
		plus rank ratio retu self sm2 sm4 sm5 tarr tateyoko tcat \
		unmap up3 yarr ycat yobi ysum zen
MANUAL=		cgi-name.txt check_attr_name.txt check_need_name.txt \
		cjoin0.txt cjoin1.txt cjoin2.txt comma.txt count.txt \
		ctail.txt delf.txt divsen.txt filehame.txt getfirst.txt \
		getlast.txt gyo.txt han.txt join0.txt join1.txt \
		join2.txt juni.txt kasan.txt keta.txt keycut.txt \
		loopj.txt loopx.txt map.txt marume.txt mdate.txt \
		mime-read.txt mojihame.txt nameread.txt plus.txt rank.txt \
		ratio.txt retu.txt self.txt sm2.txt sm4.txt sm5.txt \
		tarr.txt tateyoko.txt tcat.txt unmap.txt up3.txt yarr.txt \
		ycat.txt yobi.txt ysum.txt zen.txt
HTML=		cgi-name.html check_attr_name.html check_need_name.html \
		cjoin0.html cjoin1.html cjoin2.html comma.html count.html \
		ctail.html delf.html divsen.html filehame.html \
		getfirst.html getlast.html gyo.html han.html join0.html \
		join1.html join2.html juni.html kasan.html keta.html keycut.html \
		loopj.html loopx.html map.html marume.html mdate.html \
		mime-read.html mojihame.html nameread.html plus.html rank.html \
		ratio.html retu.html self.html sm2.html sm4.html sm5.html \
		tarr.html tateyoko.html tcat.html unmap.html up3.html yarr.html \
		ycat.html yobi.html ysum.html zen.html index.html
DOC=		INSTALL LICENSE README
PROFILE=	usp.sh

INSTALL?=	/usr/bin/install
MKDIR?=		/bin/mkdir -p
RMDIR?=		/bin/rmdir
RM?=		/bin/rm -f
CP?=		/bin/cp -f

TODAY!=		date "+%Y%m%d"

INSTALL_PROGRAM=	${INSTALL} -m ${BINMODE}
INSTALL_DOCS=		${INSTALL} -m ${DOCMODE}
BINMODE=	555
DOCMODE=	444

install:
	${MKDIR} ${BINDIR}
	@for i in ${COMMANDS}; \
	do \
		echo ${INSTALL_PROGRAM} COMMANDS/$${i} ${BINDIR}; \
		${INSTALL_PROGRAM} COMMANDS/$${i} ${BINDIR}; \
	done
	${MKDIR} ${PROFILEDIR}
	@for i in ${PROFILE}; \
	do \
		echo ${CP} deb/$${i} ${PROFILEDIR}; \
		${CP} deb/$${i} ${PROFILEDIR}; \
	done
	${MKDIR} ${DOCDIR}
	@for i in ${DOC}; \
	do \
		echo ${INSTALL_DOCS} $${i} ${DOCDIR}; \
		${INSTALL_DOCS} $${i} ${DOCDIR}; \
	done
	${MKDIR} ${MANDIR}
	@for i in ${MANUAL}; \
	do \
		echo ${INSTALL_DOCS} MANUAL/$${i} ${MANDIR}; \
		${INSTALL_DOCS} MANUAL/$${i} ${MANDIR}; \
	done
	${MKDIR} ${HTMDIR}
	@for i in ${HTML}; \
	do \
		echo ${INSTALL_DOCS} MANUALHTML/$${i} ${HTMDIR}; \
		${INSTALL_DOCS} MANUALHTML/$${i} ${HTMDIR}; \
	done
	${MKDIR} ${HTMDIR}/COMMON/CSS
	${MKDIR} ${HTMDIR}/COMMON/IMG
	${MKDIR} ${HTMDIR}/COMMON/JS
	${INSTALL_DOCS} MANUALHTML/COMMON/CSS/MAIN.CSS ${HTMDIR}/COMMON/CSS/
	${INSTALL_DOCS} MANUALHTML/COMMON/CSS/MENU.CSS ${HTMDIR}/COMMON/CSS/
	${INSTALL_DOCS} MANUALHTML/COMMON/CSS/IE8HACK.CSS ${HTMDIR}/COMMON/CSS/
	${INSTALL_DOCS} MANUALHTML/COMMON/CSS/IE7HACK.CSS ${HTMDIR}/COMMON/CSS/
	${INSTALL_DOCS} MANUALHTML/COMMON/CSS/IE6HACK.CSS ${HTMDIR}/COMMON/CSS/
	${INSTALL_DOCS} MANUALHTML/COMMON/CSS/IE67HACK.CSS ${HTMDIR}/COMMON/CSS/
	${INSTALL_DOCS} MANUALHTML/COMMON/IMG/BACKGROUND.JPG ${HTMDIR}/COMMON/IMG/
	${INSTALL_DOCS} MANUALHTML/COMMON/IMG/TUKUBAI_LOGO.JPG ${HTMDIR}/COMMON/IMG/
	${INSTALL_DOCS} MANUALHTML/COMMON/JS/MENU.JS ${HTMDIR}/COMMON/JS/
	${MKDIR} ${PDFDIR}
	${INSTALL_DOCS} MANUALPDF/all.pdf ${PDFDIR}

uninstall:
	@for i in ${COMMANDS}; \
	do \
		echo ${RM} ${BINDIR}/$${i}; \
		${RM} ${BINDIR}/$${i}; \
	done
	@for i in ${PROFILE}; \
	do \
		echo ${RM} ${PROFILEDIR}/$${i}; \
		${RM} ${PROFILEDIR}/$${i};
	done
	@for i in ${DOC}; \
	do \
		echo ${RM} ${DOCDIR}/$${i}; \
		${RM} ${DOCDIR}/$${i}; \
	done
	@for i in ${MANUAL}; \
	do \
		echo ${RM} ${MANDIR}/$${i}; \
		${RM} ${MANDIR}/$${i}; \
	done
	@for i in ${HTML}; \
	do \
		echo ${RM} ${HTMDIR}/$${i}; \
		${RM} ${HTMDIR}/$${i}; \
	done
	${RM} ${HTMDIR}/COMMON/CSS/MAIN.CSS
	${RM} ${HTMDIR}/COMMON/CSS/MENU.CSS
	${RM} ${HTMDIR}/COMMON/CSS/IE8HACK.CSS
	${RM} ${HTMDIR}/COMMON/CSS/IE7HACK.CSS
	${RM} ${HTMDIR}/COMMON/CSS/IE6HACK.CSS
	${RM} ${HTMDIR}/COMMON/CSS/IE67HACK.CSS
	${RM} ${HTMDIR}/COMMON/IMG/BACKGROUND.JPG
	${RM} ${HTMDIR}/COMMON/IMG/TUKUBAI_LOGO.JPG
	${RM} ${HTMDIR}/COMMON/JS/MENU.JS
	${RM} ${PDFDIR}/all.pdf
	${RMDIR} ${DOCDIR}
	${RMDIR} ${MANDIR}
	${RMDIR} ${HTMDIR}/COMMON/CSS
	${RMDIR} ${HTMDIR}/COMMON/IMG
	${RMDIR} ${HTMDIR}/COMMON/JS
	${RMDIR} ${HTMDIR}/COMMON
	${RMDIR} ${HTMDIR}
	${RMDIR} ${PDFDIR}
	${RMDIR} ${DATDIR}
	${RMDIR} ${TUKDIR}

deinstall: uninstall

package: clean
	${MKDIR} ${NAME}-${TODAY}
	cp -Rp COMMANDS MANUAL MANUALHTML MANUALPDF \
	   INSTALL LICENSE Makefile README \
	   ${NAME}-${TODAY}
	tar vcf ${NAME}-${TODAY}.tar ${NAME}-${TODAY}
	bzip2 ${NAME}-${TODAY}.tar

clean:
	rm -rf ${NAME}-${TODAY}*
