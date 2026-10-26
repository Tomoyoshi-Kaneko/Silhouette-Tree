# silhouetteTree(日本語)

[English README](README.md) · [仕様書 (EN/JA)](docs/specification.md)

**シルエット係数でクラスター数を決め、クラスターごとに色分けした系統樹(樹形図)を描く**ツールです。
コードを書かずにブラウザで使える Shiny アプリと、同じ処理を R から呼べる関数を用意しています。
計算は Kaneko *et al.* (J Virol 2025, doi:10.1128/jvi.01061-25) の解析スクリプトに従っています。

## できること

1. **入力**
   - 系統樹ファイル(Newick 形式: `.nwk` `.newick` `.ntw` `.tree` `.txt`、貼り付けも可)
   - 自分で測定した表(CSV / TSV / Excel、Excel からの貼り付けも可)。数値・0/1(有無)・カテゴリが混在してよい
2. **距離の計算**
   - 系統樹: 枝を辿った距離
   - 表: Gower 距離(既定)。数値列は列ごとに範囲(最大 − 最小)で割るため、**値の絶対値が大きい列に引きずられません**(正規化が内蔵)。ユークリッド / マンハッタン距離を選ぶ場合は min–max か z-score で正規化できます
3. **クラスタリングと k の決定** — `hclust`(既定 ward.D2)で k = 2〜9 を試し、**平均シルエット幅が最大の k** を提案します(手動で変更可)
4. **出力** — 色分けした樹形図 / 元の系統樹、k ごとのシルエット図、サンプルごとのシルエット図、Excel、PNG / SVG、Newick

平均シルエット幅の目安: 0.70 超 = 強い構造 / 0.51–0.70 = 妥当 / 0.26–0.50 = 弱い / 0.25 以下 = ほぼ構造なし。

## アプリの使い方

1. 「1. データの種類」で系統樹か表を選ぶ
2. ファイルを読み込む(または貼り付け、「Load demo …」のボタンで例を試す)
3. 表の場合は「サンプル名の列」を選び、「Variables」タブで各列の型(数値 / 0-1 / カテゴリ / 使わない)を確認する
4. 上部のカードに最適な k と平均シルエット幅が出る。「Silhouette by k」タブの点をクリックすると、その k に変更できる
5. 「Tree」タブから図を、「Results table」タブから Excel をダウンロードする

### 「Silhouette per sample」タブとは

採用した k における**サンプルごとのシルエット係数**の棒グラフです。
サンプル *i* について *a* = 同じクラスターの他のメンバーまでの平均距離、*b* = 最も近い別のクラスターまでの平均距離とすると、
*s* = (*b* − *a*) / max(*a*, *b*) です。1 に近いほどそのクラスターにぴったり、0 付近は 2 つのクラスターの境目、
負は別のクラスターのほうが近い(分類が怪しい)ことを表します。棒の色はクラスター、破線は全体の平均です。
1 サンプルだけのクラスターは *s* = 0 と定義されるため棒が出ません(遠い外群はこうなりやすく、平均を下げます。「Exclude samples」で除外できます)。

## R から使う

```r
library(silhouetteTree)
res <- run_silhouette_tree("tree.nwk", type = "newick")     # 系統樹
res <- run_silhouette_tree("table.xlsx", type = "table")    # 表(Gower)
res$best                    # 最適な k
st_samples_table(res)       # サンプルごとのクラスターとシルエット係数
plot_dendrogram(res)
```

詳しい仕様と例は [docs/specification.md](docs/specification.md)(日本語・英語併記)にあります。

## 自分のサーバーで公開する(GitHub → Posit Connect Cloud)

1. このフォルダの中身を GitHub リポジトリ(public、`main` ブランチ)に置く
2. 手元の R で、パッケージのルートから一度だけ(`library()` を増やしたときもう一度)実行する

   ```r
   install.packages(c("shiny", "DT", "ggplot2", "openxlsx", "readxl", "ape",
                      "cluster", "svglite", "rsconnect"))
   source("inst/shiny_app/deploy.R")    # inst/shiny_app/manifest.json ができる
   ```

   `manifest.json` も GitHub に push する
3. <https://connect.posit.cloud> で Publish → Shiny → from GitHub を選び、リポジトリとブランチ、**Primary file = `inst/shiny_app/app.R`** を指定して公開する
4. 以後は push すると自動で更新される

## 注意点

- 論文の本文は complete 法、補足コードは ward.D2 と書かれていて食い違っています。本ツールの既定は ward.D2 です(画面で変更可)。
- シルエット係数は k = 2 を選びやすい、外群が 1 個クラスターになる、といった癖があります。結果は生物学的な知見と合わせて判断してください。
- 値が全て欠損、または 0/1 列で「両方 0」しか比べられないサンプルの組は距離 0 として扱い、警告を表示します。

## AI の利用について

このリポジトリのスクリプトは、著者の過去のパッケージ susiR の構成を参考に、AI (Claude, Anthropic) の支援を受けて作成し、著者が内容を確認しました。
数値は論文の解析結果と照合しています(英語版 README の *Checks* 参照)。不具合があれば GitHub の issue でお知らせください。

## ライセンス

MIT © 2026 Tomoyoshi Kaneko
