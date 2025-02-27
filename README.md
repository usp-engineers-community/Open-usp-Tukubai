﻿               Universal Shell Programming Laboratory
                         uecinfo@usp-lab.com
                             2025/01/20
 
Open usp Tukubaiとは
====================

Open usp Tukubaiは短期間低コストで企業システムを構築するエンタープライズ向けコマンド群「usp Tukubai」のオープンソースソフトウェア版。ユニケージ開発手法の普及促進を狙い、usp Tukubaiから特に利用頻度の高いものを選定して提供されている。

Open usp Tukubaiは基幹業務システム、情報分析システム、データバッチ処理、高速検索システム、勘定系システムなどにおけるシェルおよびシェルスクリプトの用途を広げるためのUNIX環境用コマンド群であり、The MIT Licenseのもとでオープンソースソフトウェアとして公開されている。テキストデータの集計や結合 (リレーション)、その他業務システムで利用するコマンドが用意されている。

Open usp Tukubaiはターミナルから操作するインタラクティブなデータ処理作業から、シェルスクリプトによる業務システムの構築・業務用Webアプリケーションの構築まで、UNIXの基本機能を最大限に活用するプログラミング能力を提供する。

Open usp Tukubaiのサポート(有償)、商用版Tukubaiに関するお問合せ先は下記へ。

                         uecinfo@usp-lab.com

ダウンロード
===========

ダウンロードページ

- [https://www.uni-tama.com/cgi/DOCUMENT_UEC.CGI?CATEGORY=TUKUBAI&POMPA=DOWNLOAD](https://www.uni-tama.com/cgi/DOCUMENT_UEC.CGI?CATEGORY=TUKUBAI&POMPA=DOWNLOAD)

GitHub

- [https://github.com/usp-engineers-community/Open-usp-Tukubai](https://github.com/usp-engineers-community/Open-usp-Tukubai)

GitHub Read-Only

- git://github.com/usp-engineers-community/Open-usp-Tukubai.git

Haskell をインストールする
========

現在の最新版 (9.12.1)を例にインストール方法を示す。

    $ dnf install gcc gcc-c++ make tar vim wget -y
    $ wget https://downloads.haskell.org/~ghc/9.12.1/ghc-9.12.1-x86_64-rocky8-linux.tar.gz -O - |
      tar fxz - -C /usr/local/src
    $ cd /usr/local/src/ghc-9.12.1-x86_64-unknown-linux
    $ ./configure
    $ make install

インストールが済めば COMMANDS.HS ディレクトリ以下のコマンドが直接実行可能になっている。

    [root@13e8bb175b73 Open-usp-Tukubai]# COMMANDS.HS/juni.hs --version
    Usage    : juni [<f1> <f2> <file>]
    Version  : Mon Jan 20 06:55:16 UTC 2025
    Open usp Tukubai (LINUX+FREEBSD)

利用方法
========

Open usp Tukubaiはスクリプト言語Pythonで開発されている。PythonのインストールされたUNIX系OSで利用できる。Open usp Tukubaiのインストールおよびセットアップに関してはINSTALLファイルを参照のこと。

なお、業務システム向けに提供されている商用版のTukubaiはC言語で開発されている。商用版の提供や教育プログラムに興味がある場合にはuecinfo@usp-lab.comまでご連絡いただきたい。


リファレンス
============

オンラインコマンドマニュアル

- [https://www.uni-tama.com/cgi/COMMAND.CGI](https://www.uni-tama.com/cgi/COMMAND.CGI)

付属のマニュアルデフォルトインストール先は

- /usr/local/share/open-usp-tukubai/devel/pdf/
- /usr/local/share/open-usp-tukubai/devel/html/
- /usr/local/share/open-usp-tukubai/devel/man/


文献
====

- [ユニケージ魂](https://www.uni-tama.com/)
- USP MAGAZINE
- 技術評論社 Software Design


ライセンスについて
==================

Open up TukubaiはThe MIT Licenseに基づき、USP研究所が無償で配布している。The MIT Licenseの詳細についてはLICENSEファイルを参照のこと。

※ユニケージはユニバーサル・シェル・プログラミング研究所の登録商標。

