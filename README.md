# Image processing

プロジェクトルート直下に`lips`が置かれることを前提としています。
それらの`make`が終わったのちに以下を実行してください。

```
$ cd src
$ bash get-queue.sh
$ bash generate_pgm ../img/*.bmp
$ make 
$ ./run <処理したい画像(例えば../img/Lenna.pgm)> 
```


## 内容物

`src`以下のものについて記述する。

* `pnm_tools.f90`
    * 処理後の画像の表示やpgmでの書き出しを行うmodule
* `filtering.f90`
    * 画像にフィルタリングを行うサブルーチンを集めたmodule
    * 現在実装済みは以下の通り
        * gaussianフィルタ
        * sobelフィルタ
        * canny edge detection
        * bilateralフィルタ
