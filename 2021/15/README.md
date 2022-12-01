# [Day 15: Chiton](https://adventofcode.com/2021/day/15)

You've almost reached the exit of the cave, but the walls are getting closer together. Your submarine can barely still fit, though; the main problem is that the walls of the cave are covered in [chitons](https://en.wikipedia.org/wiki/Chiton), and it would be best not to bump any of them.

The cavern is large, but has a very low ceiling, restricting your motion to two dimensions. The shape of the cavern resembles a square; a quick scan of chiton density produces a map of **risk level** throughout the cave (your puzzle input). For example:

```
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
```

You start in the top left position, your destination is the bottom right position, and you cannot move diagonally. The number at each position is its **risk level**; to determine the total risk of an entire path, add up the risk levels of each position you **enter** (that is, don't count the risk level of your starting position unless you enter it; leaving it adds no risk to your total).

Your goal is to find a path with the **lowest total risk**. In this example, a path with the lowest total risk is highlighted here:

<pre><code><strong>1</strong>163751742
<strong>1</strong>381373672
<strong>2136511</strong>328
369493<strong>15</strong>69
7463417<strong>1</strong>11
1319128<strong>13</strong>7
13599124<strong>2</strong>1
31254216<strong>3</strong>9
12931385<strong>21</strong>
231194458<strong>1</strong>
</code></pre>

The total risk of this path is **`40`** (the starting position is never entered, so its risk is not counted).

**What is the lowest total risk of any path from the top left to the bottom right?**

## Part Two

Now that you know how to find low-risk paths in the cave, you can try to find your way out.

The entire cave is actually **five times larger in both dimensions** than you thought; the area you originally scanned is just one tile in a 5x5 tile area that forms the full map. Your original map tile repeats to the right and downward; each time the tile repeats to the right or downward, all of its risk levels **are 1 higher** than the tile immediately up or left of it. However, risk levels above `9` wrap back around to `1`. So, if your original map had some position with a risk level of `8`, then that same position on each of the 25 total tiles would be as follows:

```
8 9 1 2 3
9 1 2 3 4
1 2 3 4 5
2 3 4 5 6
3 4 5 6 7
```

Each single digit above corresponds to the example position with a value of `8` on the top-left tile. Because the full map is actually five times larger in both dimensions, that position appears a total of 25 times, once in each duplicated tile, with the values shown above.

Here is the full five-times-as-large version of the first example above, with the original map in the top left corner highlighted:

<pre><code><strong>1163751742</strong>2274862853338597396444961841755517295286
<strong>1381373672</strong>2492484783351359589446246169155735727126
<strong>2136511328</strong>3247622439435873354154698446526571955763
<strong>3694931569</strong>4715142671582625378269373648937148475914
<strong>7463417111</strong>8574528222968563933317967414442817852555
<strong>1319128137</strong>2421239248353234135946434524615754563572
<strong>1359912421</strong>2461123532357223464346833457545794456865
<strong>3125421639</strong>4236532741534764385264587549637569865174
<strong>1293138521</strong>2314249632342535174345364628545647573965
<strong>2311944581</strong>3422155692453326671356443778246755488935
22748628533385973964449618417555172952866628316397
24924847833513595894462461691557357271266846838237
32476224394358733541546984465265719557637682166874
47151426715826253782693736489371484759148259586125
85745282229685639333179674144428178525553928963666
24212392483532341359464345246157545635726865674683
24611235323572234643468334575457944568656815567976
42365327415347643852645875496375698651748671976285
23142496323425351743453646285456475739656758684176
34221556924533266713564437782467554889357866599146
33859739644496184175551729528666283163977739427418
35135958944624616915573572712668468382377957949348
43587335415469844652657195576376821668748793277985
58262537826937364893714847591482595861259361697236
96856393331796741444281785255539289636664139174777
35323413594643452461575456357268656746837976785794
35722346434683345754579445686568155679767926678187
53476438526458754963756986517486719762859782187396
34253517434536462854564757396567586841767869795287
45332667135644377824675548893578665991468977611257
44961841755517295286662831639777394274188841538529
46246169155735727126684683823779579493488168151459
54698446526571955763768216687487932779859814388196
69373648937148475914825958612593616972361472718347
17967414442817852555392896366641391747775241285888
46434524615754563572686567468379767857948187896815
46833457545794456865681556797679266781878137789298
64587549637569865174867197628597821873961893298417
45364628545647573965675868417678697952878971816398
56443778246755488935786659914689776112579188722368
55172952866628316397773942741888415385299952649631
57357271266846838237795794934881681514599279262561
65719557637682166874879327798598143881961925499217
71484759148259586125936169723614727183472583829458
28178525553928963666413917477752412858886352396999
57545635726865674683797678579481878968159298917926
57944568656815567976792667818781377892989248891319
75698651748671976285978218739618932984172914319528
56475739656758684176786979528789718163989182927419
67554889357866599146897761125791887223681299833479</code></pre>

Equipped with the full map, you can now find a path from the top left corner to the bottom right corner with the lowest total risk:

<pre><code><strong>1</strong>1637517422274862853338597396444961841755517295286
<strong>1</strong>3813736722492484783351359589446246169155735727126
<strong>2</strong>1365113283247622439435873354154698446526571955763
<strong>3</strong>6949315694715142671582625378269373648937148475914
<strong>7</strong>4634171118574528222968563933317967414442817852555
<strong>1</strong>3191281372421239248353234135946434524615754563572
<strong>1</strong>3599124212461123532357223464346833457545794456865
<strong>3</strong>1254216394236532741534764385264587549637569865174
<strong>1</strong>2931385212314249632342535174345364628545647573965
<strong>2</strong>3119445813422155692453326671356443778246755488935
<strong>2</strong>2748628533385973964449618417555172952866628316397
<strong>2</strong>4924847833513595894462461691557357271266846838237
<strong>324</strong>76224394358733541546984465265719557637682166874
47<strong>15</strong>1426715826253782693736489371484759148259586125
857<strong>4</strong>5282229685639333179674144428178525553928963666
242<strong>1</strong>2392483532341359464345246157545635726865674683
246<strong>1123532</strong>3572234643468334575457944568656815567976
423653274<strong>1</strong>5347643852645875496375698651748671976285
231424963<strong>2342</strong>5351743453646285456475739656758684176
342215569245<strong>332</strong>66713564437782467554889357866599146
33859739644496<strong>1</strong>84175551729528666283163977739427418
35135958944624<strong>61</strong>6915573572712668468382377957949348
435873354154698<strong>44</strong>652657195576376821668748793277985
5826253782693736<strong>4</strong>893714847591482595861259361697236
9685639333179674<strong>1</strong>444281785255539289636664139174777
3532341359464345<strong>2461</strong>575456357268656746837976785794
3572234643468334575<strong>4</strong>579445686568155679767926678187
5347643852645875496<strong>3</strong>756986517486719762859782187396
3425351743453646285<strong>4564</strong>757396567586841767869795287
4533266713564437782467<strong>554</strong>8893578665991468977611257
449618417555172952866628<strong>3163</strong>9777394274188841538529
462461691557357271266846838<strong>2</strong>3779579493488168151459
546984465265719557637682166<strong>8</strong>7487932779859814388196
693736489371484759148259586<strong>125</strong>93616972361472718347
17967414442817852555392896366<strong>6413</strong>91747775241285888
46434524615754563572686567468379<strong>7</strong>67857948187896815
46833457545794456865681556797679<strong>26</strong>6781878137789298
645875496375698651748671976285978<strong>21</strong>873961893298417
4536462854564757396567586841767869<strong>7</strong>952878971816398
5644377824675548893578665991468977<strong>6112</strong>579188722368
5517295286662831639777394274188841538<strong>5</strong>299952649631
5735727126684683823779579493488168151<strong>4</strong>599279262561
6571955763768216687487932779859814388<strong>1</strong>961925499217
7148475914825958612593616972361472718<strong>34725</strong>83829458
28178525553928963666413917477752412858886<strong>3</strong>52396999
57545635726865674683797678579481878968159<strong>2</strong>98917926
57944568656815567976792667818781377892989<strong>24</strong>8891319
756986517486719762859782187396189329841729<strong>1431</strong>9528
564757396567586841767869795287897181639891829<strong>2</strong>7419
675548893578665991468977611257918872236812998<strong>33479</strong></code></pre>

The total risk of this path is **`315`** (the starting position is still never entered, so its risk is not counted).

Using the full map, **what is the lowest total risk of any path from the top left to the bottom right?**