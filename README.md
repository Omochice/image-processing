# Image processing

プロジェクトルート直下に`lips`が置かれることを前提としています。
それらの`make`が終わったのちに
```
$ cd src
$ make 
$ bash generate_pgm ../img/*.bmp
$ ./run <処理したい画像(例えば../img/Lenna.pgm)> 
```

