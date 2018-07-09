# The MIT License
#
# Copyright (C) 2012-2014 Universal Shell Programming Laboratory
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

PREFIX?=	/usr/local
BINDIR?=	${PREFIX}/bin
TUKDIR?=	${PREFIX}/share/${NAME}
DATDIR?=	${TUKDIR}/devel
MANDIR=		${DATDIR}/man
HTMDIR=		${DATDIR}/html
PDFDIR=		${DATDIR}/pdf
DOCDIR=		${DATDIR}/doc

COMMANDS=	calclock cgi-name check_attr_name check_need_name cjoin0 cjoin1 cjoin2 comma count ctail dayslash delf divsen filehame formhame getfirst getlast gyo han join0 join1 join2 juni kasan keta keycut loopj loopx maezero man2 map marume mdate mime-read mojihame nameread numchar plus rank ratio retu self sm2 sm4 sm5 tarr tateyoko tcat unmap up3 yarr ycat yobi ysum zen
MANUAL=		calclock.txt cgi-name.txt check_attr_name.txt check_need_name.txt cjoin0.txt cjoin1.txt cjoin2.txt comma.txt count.txt ctail.txt dayslash.txt delf.txt divsen.txt field-format.txt filehame.txt formhame.txt getfirst.txt getlast.txt gyo.txt han.txt join0.txt join1.txt join2.txt juni.txt kasan.txt keta.txt keycut.txt lcalc.txt lineup.txt loopj.txt loopx.txt maezero.txt map.txt marume.txt master-file.txt mdate.txt mime-read.txt mojihame.txt name-file.txt name-format.txt nameread.txt numchar.txt plus.txt rank.txt ratio.txt retu.txt self.txt sisu.txt sm2.txt sm4.txt sm5.txt sorter.txt tag-format.txt tarr.txt tateyoko.txt tcat.txt transaction-file.txt unmap.txt up3.txt yarr.txt ycat.txt yobi.txt ysum.txt zen.txt
HTML=		calclock.html cgi-name.html check_attr_name.html check_need_name.html cjoin0.html cjoin1.html cjoin2.html comma.html count.html ctail.html dayslash.html delf.html divsen.html field-format.html filehame.html formhame.html getfirst.html getlast.html gyo.html han.html index.html join0.html join1.html join2.html juni.html kasan.html keta.html keycut.html lcalc.html lineup.html loopj.html loopx.html maezero.html map.html marume.html master-file.html mdate.html mime-read.html mojihame.html name-file.html name-format.html nameread.html plus.html rank.html ratio.html retu.html self.html sisu.html sm2.html sm4.html sm5.html sorter.html tag-format.html tarr.html tateyoko.html tcat.html transaction-file.html unmap.html up3.html yarr.html ycat.html yobi.html ysum.html zen.html
DOC=		INSTALL LICENSE README README.md

INSTALL?=	/usr/bin/install
MKDIR?=		/bin/mkdir -p
RMDIR?=		/bin/rmdir
RM?=		/bin/rm -f

TODAY!=		date "+%Y%m%d"

INSTALL_PROGRAM=${INSTALL} -m ${BINMODE}
INSTALL_DOCS=	${INSTALL} -m ${DOCMODE}
BINMODE=	555
DOCMODE=	444

all:
	@echo "Run 'make install' to install"
	@echo "    PREFIX=${PREFIX}"
	@echo "    BINDIR=${BINDIR}"
	@echo "    TUKDIR=${TUKDIR}"
	@echo "    DATDIR=${TUKDIR}/devel"
	@echo "    MANDIR=${DATDIR}/man"
	@echo "    HTMDIR=${DATDIR}/html"
	@echo "    PDFDIR=${DATDIR}/pdf"
	@echo "    DOCDIR=${DATDIR}/doc"

install:
	${MKDIR} ${DESTDIR}${BINDIR}
	@for i in ${COMMANDS}; \
	do \
		echo ${INSTALL_PROGRAM} COMMANDS/$${i} ${DESTDIR}${BINDIR}; \
		${INSTALL_PROGRAM} COMMANDS/$${i} ${DESTDIR}${BINDIR}; \
	done
	${MKDIR} ${DESTDIR}${DOCDIR}
	@for i in ${DOC}; \
	do \
		echo ${INSTALL_DOCS} $${i} ${DESTDIR}${DOCDIR}; \
		${INSTALL_DOCS} $${i} ${DESTDIR}${DOCDIR}; \
	done
	${MKDIR} ${DESTDIR}${MANDIR}
	@for i in ${MANUAL}; \
	do \
		echo ${INSTALL_DOCS} MANUAL/$${i} ${DESTDIR}${MANDIR}; \
		${INSTALL_DOCS} MANUAL/$${i} ${DESTDIR}${MANDIR}; \
	done
	${MKDIR} ${DESTDIR}${HTMDIR}
	@for i in ${HTML}; \
	do \
		echo ${INSTALL_DOCS} MANUALHTML/$${i} ${DESTDIR}${HTMDIR}; \
		${INSTALL_DOCS} MANUALHTML/$${i} ${DESTDIR}${HTMDIR}; \
	done
	${MKDIR} ${DESTDIR}${HTMDIR}/COMMON/CSS
	${MKDIR} ${DESTDIR}${HTMDIR}/COMMON/IMG
	${MKDIR} ${DESTDIR}${HTMDIR}/COMMON/JS
	${INSTALL_DOCS} MANUALHTML/COMMON/CSS/MAIN.CSS ${DESTDIR}${HTMDIR}/COMMON/CSS/
	${INSTALL_DOCS} MANUALHTML/COMMON/CSS/MENU.CSS ${DESTDIR}${HTMDIR}/COMMON/CSS/
	${INSTALL_DOCS} MANUALHTML/COMMON/CSS/PRINT.CSS ${DESTDIR}${HTMDIR}/COMMON/CSS/
	${INSTALL_DOCS} MANUALHTML/COMMON/CSS/IE7HACK.CSS ${DESTDIR}${HTMDIR}/COMMON/CSS/
	${INSTALL_DOCS} MANUALHTML/COMMON/CSS/IE6HACK.CSS ${DESTDIR}${HTMDIR}/COMMON/CSS/
	${INSTALL_DOCS} MANUALHTML/COMMON/CSS/IE67HACK.CSS ${DESTDIR}${HTMDIR}/COMMON/CSS/
	${INSTALL_DOCS} MANUALHTML/COMMON/IMG/BACKGROUND.JPG ${DESTDIR}${HTMDIR}/COMMON/IMG/
	${INSTALL_DOCS} MANUALHTML/COMMON/IMG/TUKUBAI_LOGO.JPG ${DESTDIR}${HTMDIR}/COMMON/IMG/
	${INSTALL_DOCS} MANUALHTML/COMMON/JS/MENU.JS ${DESTDIR}${HTMDIR}/COMMON/JS/
	${MKDIR} ${DESTDIR}${PDFDIR}
	${INSTALL_DOCS} MANUALPDF/all.pdf ${DESTDIR}${PDFDIR}

uninstall:
	@for i in ${COMMANDS}; \
	do \
		echo ${RM} ${DESTDIR}${BINDIR}/$${i}; \
		${RM} ${DESTDIR}${BINDIR}/$${i}; \
	done
	@for i in ${DOC}; \
	do \
		echo ${RM} ${DESTDIR}${DOCDIR}/$${i}; \
		${RM} ${DESTDIR}${DOCDIR}/$${i}; \
	done
	@for i in ${MANUAL}; \
	do \
		echo ${RM} ${DESTDIR}${MANDIR}/$${i}; \
		${RM} ${DESTDIR}${MANDIR}/$${i}; \
	done
	@for i in ${HTML}; \
	do \
		echo ${RM} ${DESTDIR}${HTMDIR}/$${i}; \
		${RM} ${DESTDIR}${HTMDIR}/$${i}; \
	done
	${RM} ${DESTDIR}${HTMDIR}/COMMON/CSS/MAIN.CSS
	${RM} ${DESTDIR}${HTMDIR}/COMMON/CSS/MENU.CSS
	${RM} ${DESTDIR}${HTMDIR}/COMMON/CSS/PRINT.CSS
	${RM} ${DESTDIR}${HTMDIR}/COMMON/CSS/IE7HACK.CSS
	${RM} ${DESTDIR}${HTMDIR}/COMMON/CSS/IE6HACK.CSS
	${RM} ${DESTDIR}${HTMDIR}/COMMON/CSS/IE67HACK.CSS
	${RM} ${DESTDIR}${HTMDIR}/COMMON/IMG/BACKGROUND.JPG
	${RM} ${DESTDIR}${HTMDIR}/COMMON/IMG/TUKUBAI_LOGO.JPG
	${RM} ${DESTDIR}${HTMDIR}/COMMON/JS/MENU.JS
	${RM} ${DESTDIR}${PDFDIR}/all.pdf
	${RMDIR} ${DESTDIR}${DOCDIR}
	${RMDIR} ${DESTDIR}${MANDIR}
	${RMDIR} ${DESTDIR}${HTMDIR}/COMMON/CSS
	${RMDIR} ${DESTDIR}${HTMDIR}/COMMON/IMG
	${RMDIR} ${DESTDIR}${HTMDIR}/COMMON/JS
	${RMDIR} ${DESTDIR}${HTMDIR}/COMMON
	${RMDIR} ${DESTDIR}${HTMDIR}
	${RMDIR} ${DESTDIR}${PDFDIR}
	${RMDIR} ${DESTDIR}${DATDIR}
	${RMDIR} ${DESTDIR}${TUKDIR}

deinstall: uninstall

package: clean
	${MKDIR} ${NAME}-${TODAY}
	cp -Rp COMMANDS MANUAL MANUALHTML MANUALPDF INSTALL LICENSE Makefile README README.md ${NAME}-${TODAY}
	tar vcf ${NAME}-${TODAY}.tar ${NAME}-${TODAY}
	bzip2 ${NAME}-${TODAY}.tar
	${RM} ${NAME}-${TODAY}

clean:
	rm -rf ${NAME}-${TODAY}*

.PHONY: install
