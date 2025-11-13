# 💻 GnuCOBOL（OpenCOBOL）バッチ処理サンプル集

## 📝 プロジェクト概要

このプロジェクトは、**GnuCOBOL (GNU OpenCOBOL)** を用いて開発される、**一般的なビジネスバッチ処理**のパターンを網羅したサンプルコード集です。

COBOL学習者が実務で遭遇する頻度の高い、「ファイルの突き合わせ（マッチング）」「データ集計」「データ抽出」などの基本的なバッチ処理のロジックを、すぐに実行可能な形で提供します。

## 🎯 目的

* GnuCOBOLによる**実用的なバッチ処理ロジック**の理解を深める。
* COBOLの**ファイル処理（順ファイル、索引ファイル）** や **SORT文** の具体的な使用方法を学ぶ。
* 実務開発における**リファレンス**として活用する。

## 🚀 収録されているサンプル（抜粋）

主要なバッチ処理のカテゴリ別にサンプルを分類しています。

| カテゴリ | サンプル名 | 概要 |
| :--- | :--- | :--- |
| **マッチング** | `MATCH01.cbl` | マスタファイルとトランザクションファイルの**キー照合（追加/更新/削除）**。 |
| **集計** | `SUMM01.cbl` | 入力ファイルから**キーブレイク**による売上集計を行い、合計値を算出。 |
| **抽出/選択**| `SELECT01.cbl` | 条件に合致するレコードのみを抽出し、**エラーリスト**を作成。 |
| **変換** | `REFORMAT01.cbl` | 固定長ファイルを読み込み、項目を並べ替えて別の固定長ファイルに出力。 |
| **ソート** | `SORT01.cbl` | COBOLの**SORT文**を使用してファイルをソートする基本処理。 |
| **マスター更新** | `SALES_PROC.cbl` | 売上データに基づき**索引マスターファイル**を更新。**サブルーチンコール**や**COPY句**も使用。 |

---

## 🛠️ 環境構築と実行方法

### 1. 必要な環境

* **GnuCOBOLコンパイラ**: バージョン 3.0 以降を推奨します。
    * インストール方法: `sudo apt install gnucobol` (Linuxの場合)
* **make**: ビルド自動化ツール。
    * インストール方法: `sudo apt install make` (Linuxの場合)

### 2. ビルドと実行

1.  **ビルド**
    プロジェクトのルートディレクトリで`make`コマンドを実行すると、全てのサンプルプログラムがコンパイルされ、実行可能ファイルが`bin/`ディレクトリに生成されます。
    ```bash
    make
    ```

2.  **実行**
    ビルドが完了したら、`bin/`ディレクトリ内の実行可能ファイルを直接実行します。
    ```bash
    # 例：SUMM01を実行する場合
    ./bin/SUMM01
    ```
    * 各サンプルが必要とする入力データは、`data/`ディレクトリ以下に配置されています。

3.  **クリーン**
    ビルドで生成された全ての実行可能ファイルを削除するには、`make clean`コマンドを実行します。
    ```bash
    make clean
    ```

### 3. データ定義について

全てのサンプルプログラムは、プログラム内で定義された以下の構造を基本とします。

* **ファイル定義**: `SELECT ... ASSIGN TO ...`
* **レコード定義**: `FD`句および`01`レベルの項目定義

---

## 📁 ファイル配置

本プロジェクトは、ソースコードとデータを明確に分離するために、以下のディレクトリ構成を採用しています。

*   **`src/`**: COBOLソースコード（`.cbl`）およびコンパイル後の実行可能ファイルを格納します。
    *   機能ごとにサブディレクトリ（例: `matching`, `aggregation`）を作成し、関連ファイルをまとめて管理します。
*   **`data/`**: プログラムが使用する入力データファイル、およびプログラムが出力するファイルを格納します。

---

## 📊 データファイルレイアウト

`data/` ディレクトリに配置されている各ファイルの役割は以下の通りです。

| ファイルパス | I/O | 関連プログラム | 概要 |
| :--- | :--- | :--- | :--- |
| `data/matching/MASTER.DAT` | 入力 | `MATCH01.cbl` | マッチング処理のマスターファイル |
| `data/matching/TRAN.DAT` | 入力 | `MATCH01.cbl` | マッチング処理のトランザクションファイル |
| `data/matching/NEWMAST.DAT` | 出力 | `MATCH01.cbl` | マッチング処理後の新マスターファイル |
| `data/matching/ERROR.LST` | 出力 | `MATCH01.cbl` | マッチング処理のエラーリスト |
| `data/aggregation/INPUT_SUMM.DAT` | 入力 | `SUMM01.cbl` | 集計処理の入力データ |
| `data/aggregation/OUTPUT_SUMM.DAT`| 出力 | `SUMM01.cbl` | 集計処理の出力結果 |
| `data/aggregation/CONTROL_LIST.DAT`| 出力 | `SUMM01.cbl` | 集計処理のコントロールリスト（件数・合計） |
| `data/selection/INPUT_SELECT.DAT`| 入力 | `SELECT01.cbl` | 抽出処理の入力データ |
| `data/selection/OUTPUT_SELECT.DAT`| 出力 | `SELECT01.cbl` | 抽出処理の出力結果 |
| `data/selection/ERROR_SELECT.LST`| 出力 | `SELECT01.cbl` | 抽出処理のエラーリスト |
| `data/reformatting/INPUT-REFORMAT.DAT`| 入力 | `REFORMAT01.cbl` | 変換処理の入力データ |
| `data/reformatting/OUTPUT-REFORMAT.DAT`| 出力 | `REFORMAT01.cbl` | 変換処理の出力結果 |
| `data/sorting/INPUT-SORT.DAT`| 入力 | `SORT01.cbl` | ソート処理の入力データ |
| `data/sorting/OUTPUT-SORT.DAT`| 出力 | `SORT01.cbl` | ソート処理の出力結果 |
| `data/master_update/initial_master.dat` | 入力 | `CREATE_MASTER.cbl` | マスターファイル作成の入力データ |
| `data/master_update/DAILY_SALES.DAT` | 入力 | `SALES_PROC.cbl` | マスター更新処理の日次売上データ |
| `data/master_update/MASTER_ITEM.IDX` | 入力/更新 | `SALES_PROC.cbl`, `INV_UPDATE.cbl`, `CREATE_MASTER.cbl` | 商品マスター（**索引ファイル**） |
| `data/master_update/SALES_HISTORY.DAT`| 出力 | `SALES_PROC.cbl` | 正常処理された売上データの履歴 |
| `data/master_update/ERROR_SALES.LST` | 出力 | `SALES_PROC.cbl` | 検証エラーとなった売上データ |
