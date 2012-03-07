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

LOCALBASE?=	/usr/local
BINDIR?=	${LOCALBASE}/bin
DATDIR?=	${LOCALBASE}/share/open-usp-tukubai/devel
MANDIR=		${DATDIR}/man
DOCDIR=		${DATDIR}/doc

COMMANDS=	cgi-name check_attr_name check_need_name comma count ctail \
		delf divsen getfirst getlast gyo han join0 join1 join2 juni \
		kasan keta keycut loopj loopx map marume mdate mime-read \
		mojihame nameread plus rank ratio retu self sm2 sm4 sm5 \
		tarr tateyoko tcat unmap up3 yarr ycat yobi ysum zen
MANUAL=		cgi-name.txt check_attr_name.txt check_need_name.txt \
		comma.txt count.txt ctail.txt delf.txt divsen.txt \
		getfirst.txt getlast.txt gyo.txt han.txt join0.txt \
		join1.txt join2.txt juni.txt kasan.txt keta.txt keycut.txt \
		loopj.txt loopx.txt map.txt marume.txt mdate.txt \
		mime-read.txt mojihame.txt nameread.txt plus.txt rank.txt \
		ratio.txt retu.txt self.txt sm2.txt sm4.txt sm5.txt \
		tarr.txt tateyoko.txt tcat.txt unmap.txt up3.txt yarr.txt \
		ycat.txt yobi.txt ysum.txt zen.txt
DOC=		INSTALL LICENSE README

INSTALL?=	/usr/bin/install
MKDIR?=		/bin/mkdir -p
RMDIR?=		/bin/rmdir
RM?=		/bin/rm

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
	${MKDIR} ${MANDIR}
	@for i in ${MANUAL}; \
	do \
		echo ${INSTALL_DOCS} MANUAL/$${i} ${MANDIR}; \
		${INSTALL_DOCS} MANUAL/$${i} ${MANDIR}; \
	done
	${MKDIR} ${DOCDIR}
	@for i in ${DOC}; \
	do \
		echo ${INSTALL_DOCS} $${i} ${DOCDIR}; \
		${INSTALL_DOCS} $${i} ${DOCDIR}; \
	done

uninstall:
	@for i in ${COMMANDS}; \
	do \
		echo ${RM} ${BINDIR}/$${i}; \
		${RM} ${BINDIR}/$${i}; \
	done
	@for i in ${MANUAL}; \
	do \
		echo ${RM} ${MANDIR}/$${i}; \
		${RM} ${MANDIR}/$${i}; \
	done
	@for i in ${DOC}; \
	do \
		echo ${RM} ${DOCDIR}/$${i}; \
		${RM} ${DOCDIR}/$${i}; \
	done
	${RMDIR} ${DOCDIR}
	${RMDIR} ${MANDIR}
	${RMDIR} ${DATDIR}

deinstall: uninstall
