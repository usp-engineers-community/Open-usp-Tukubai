# The MIT License
#
# Copyright (C) 2012-2023 Universal Shell Programming Laboratory
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

COMMANDS=1bai block_getlast calclock calsed cap cgi-name check_attr_name check_cmp_name check_date_name check_dble_name check_inlist_name check_need_name cjoin0 cjoin1 cjoin1x cjoin2 cjoin2x comma count ctail dayslash delf delr divsen exist extname filehame formhame fpad fromcsv fsed getfirst getlast gyo haba han ido isdate join0 join1 join1x join2 join2x joinx juni kasan keta keycut loopj loopx maezero man2 map marume mdate mime-read mojihame msort nameread numchar overlay plus rank ratio retu rjson self selr sm2 sm4 sm5 tarr tateyoko tcat tocsv uconv ulock unmap up3 wjson xmldir yarr ycat yobi ysum zen

MANUAL=1bai.txt block_getlast.txt calclock.txt calsed.txt cap.txt cgi-name.txt check_attr_name.txt check_cmp_name.txt check_date_name.txt check_dble_name.txt check_inlist_name.txt check_need_name.txt cjoin0.txt cjoin1.txt cjoin1x.txt cjoin2.txt cjoin2x.txt comma.txt count.txt ctail.txt dayslash.txt delf.txt delr.txt divsen.txt exist.txt extname.txt filehame.txt formhame.txt fpad.txt fromcsv.txt fsed.txt getfirst.txt getlast.txt gyo.txt haba.txt han.txt ido isdate.txt join0.txt join1.txt join1x.txt join2.txt join2x.txt joinx.txt juni.txt kasan.txt keta.txt keycut.txt loopj.txt loopx.txt maezero.txt man2.txt map.txt marume.txt mdate.txt mime-read.txt mojihame.txt msort.txt nameread.txt numchar.txt overlay.txt plus.txt rank.txt ratio.txt retu.txt rjson.txt self.txt selr.txt sm2.txt sm4.txt sm5.txt tarr.txt tateyoko.txt tcat.txt tocsv.txt uconv.txt ulock.txt unmap.txt up3.txt wjson.txt xmldir.txt yarr.txt ycat.txt yobi.txt ysum.txt zen.txt

HTML=calclock.html cgi-name.html check_attr_name.html check_need_name.html cjoin0.html cjoin1.html cjoin2.html comma.html count.html ctail.html dayslash.html delf.html divsen.html field-format.html filehame.html formhame.html fpad.html fromcsv.html getfirst.html getlast.html gyo.html han.html index.html join0.html join1.html join2.html joinx.html juni.html kasan.html keta.html keycut.html loopj.html loopx.html maezero.html map.html marume.html master-file.html mdate.html mime-read.html mojihame.html msort.html name-file.html name-format.html nameread.html overlay.html plus.html rank.html ratio.html retu.html rjson.html self.html sm2.html sm4.html sm5.html tag-format.html tarr.html tateyoko.html tcat.html tocsv.html transaction-file.html uconv.html unmap.html up3.html wjson.html xmldir.html yarr.html ycat.html yobi.html ysum.html zen.html

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

# RPM の情報

define SPECFILE
Summary:     an open source Python implementation of Tukubai.
Summary(ja): 製品版 usp Tukubai の Python によるオープンソース実装。
Name:        open-usp-tukubai
Version:     $(shell git log --date=format-local:"%Y%m%d" -1 --pretty=%cd COMMANDS)
Release:     1
License:     $(shell head -1 LICENSE)

%install cp -pr "$(shell pwd)/COMMANDS" "${BINDIR}"

%description
Open usp Tukubaiは短期間低コストで企業システムを構築するエン
タープライズ向けコマンド群「usp Tukubai」のオープンソースソ
フトウェア版。ユニケージ開発手法の普及促進を狙い、usp Tukubai
から特に利用頻度の高いものを選定して提供されている。
endef

export SPECFILE

all:
	@echo "Run 'make install' to install"
	@echo "    PREFIX=${PREFIX}"
	@echo "    BINDIR=${BINDIR}"
	@echo "    TUKDIR=${TUKDIR}"
	@echo "    DATDIR=${DATDIR}"
	@echo "    MANDIR=${MANDIR}"
	@echo "    HTMDIR=${HTMDIR}"
	@echo "    PDFDIR=${PDFDIR}"
	@echo "    DOCDIR=${DOCDIR}"

test: # シェル・スクリプトのみテストを実行する。
	@path=$$(mktemp -d); \
	echo > /dev/null "gsed が存在する場合は sed の代わりに使用する。(macOS で実行している場合のみ)"; \
	uname | awk '$$0 == "Darwin" {exit 1}' || ln -s $$(which gsed) $$path/sed; \
	for test_script in $$(echo TEST/* | tr ' ' '\n' | grep -e '\.test$$'); do \
		PATH=$$path:$$PATH "$${test_script}" "$$(pwd)/COMMANDS"; \
		if [ "$$?" -ne 0 ]; then \
			echo "test for $$(basename "$${test_script}" .sh)" failed.; \
			exit 1; \
		fi; \
	done; \
	echo Tests are finished successfully.; \
	rm -r "$$path";

rpm:
	@if ! type &> /dev/null rpmbuild; then                              \
		@echo rpmbuild コマンドが存在しません;                      \
		exit 1;                                                     \
	fi;                                                                 \
	                                                                    \
	specfile="$$(mktemp --suffix=.spec)";                               \
	: 最後に specfile は削除する。;                                     \
	trap "rm '$${specfile}'" EXIT;                                      \
	                                                                    \
	cat > "$${specfile}" <<< "$$SPECFILE";                              \
	rpmbuild -bb --clean "$${specfile}"

pdf: $(wildcard COMMANDS/*)
	@type wkhtmltopdf 2>&1 > /dev/null || (echo wkhtmltopdf must be installed. >&2; exit 1)
	@echo Let us make MANUALPDF from $$(awk '{print NF}' <<< "$+") documents.
	 backcover=$$(mktemp).html;                                                                    \
	 echo "$$PDF_BACKCOVER" | sed -e "s/<!--YEAR-->/$$(date +%Y)/" > "$${backcover}";              \
	 last_updated=$$(git log --date=format-local:"%Y年%m月%d日" -1 --pretty=%cd COMMANDS);         \
	 sed -e "s/<!--DATE-->/"$${last_updated}"/" <<< "$$PDF_COVER" |                                \
	 wkhtmltopdf --enable-local-file-access --print-media-type --footer-center [page]              \
	 	--title "Open usp Tukubai コマンドマニュアル"                                              \
	 	- $(addsuffix .html,$(addprefix MANUALHTML/,$(patsubst COMMANDS/%,%,$+)))                  \
	 	MANUALHTML/tag-format.html MANUALHTML/name-format.html MANUALHTML/field-format.html        \
	 	MANUALHTML/master-file.html MANUALHTML/name-file.html MANUALHTML/transaction-file.html     \
	 	"$${backcover}" MANUALPDF/all.pdf;                                                         \
	 rm "$${backcover}"
	@echo completed!

man:
	@for man in MANUAL/*.txt; do \
		command_name="$$(basename "$${man}" .txt)";    \
		target="man/$${command_name}.1"; \
		printf "building %s\n" "$${target}" >&2; \
	 	./MANUAL2man "$${command_name}" < "$${man}" > "$${target}"; \
	 done

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
	${INSTALL_DOCS} MANUALHTML/COMMON/IMG/BACKGROUND.JPG ${DESTDIR}${HTMDIR}/COMMON/IMG/
	${INSTALL_DOCS} MANUALHTML/COMMON/IMG/TUKUBAI_LOGO.JPG ${DESTDIR}${HTMDIR}/COMMON/IMG/
	${INSTALL_DOCS} MANUALHTML/COMMON/JS/MENU.JS ${DESTDIR}${HTMDIR}/COMMON/JS/

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
	date=$$(git log --date=format-local:"%Y%m%d" -1 --pretty=%cd COMMANDS);                                        \
	working_directory=$$(mktemp -d);                                                                               \
	package_directory=$${working_directory}/open-usp-tukubai-$${date};                                             \
	mkdir "$${package_directory}";                                                                                 \
	cp -Rp COMMANDS MANUAL MANUALHTML MANUALPDF INSTALL LICENSE Makefile README README.md TEST                     \
	       "$${package_directory}";                                                                                \
	pushd "$${working_directory}";                                                                                 \
	tar vcf "$${working_directory}.tar" .;                                                                         \
	popd;                                                                                                          \
	bzip2 "$${working_directory}.tar";                                                                             \
	cp -p "$${working_directory}.tar.bz2" open-usp-tukubai-$${date}.tar.bz2;                                       \
	rm -rf $${working_directory}

clean:
	rm -rf ${NAME}-${TODAY}*

define PDF_COVER
<meta charset="utf-8">
<style>
html {
	text-align:  center;
}

body {
	position: absolute;
	top: 50%;
	margin-top: -240px;
	width: 100%;
	height: 340px;
}

h1 {
	font: 36px serif;
	margin-bottom: 270px;
}

h2 {
	font-weight: normal;
	font-size: 20px;
}

h1 + h2 {
	margin-bottom: 72px;
}
</style>
<body>
	<h1>Open usp Tukubaiコマンドマニュアル</h1>
	<h2><!--DATE--></h2>
	<h2>ユニバーサル・シェル・プログラミング研究所</h2>
</body>
endef

define PDF_BACKCOVER
<meta charset="utf-8">
<style>
html {
	text-align:  center;
}

body {
	position: absolute;
	margin-top: -1.5em;
	top: 50%;
	width: 100%;
	font-size: 14px;
	line-height: 2em;
}
</style>
<body>
	Contact us: uecinfo@usp-lab.com<br>
	Copyright &copy; 2012-<!--YEAR--> Universal Shell Programming Laboratory All Rights Reserved.
</body>
endef

export PDF_COVER
export PDF_BACKCOVER

.PHONY: man install test
