Tartalom:

ksatrak.pl	Prolog keretprogram
prolog_idok.txt	Prolog futási idők
README.txt	Leírás (ez a fájl)
tests/		Tesztesetek és megoldások

A keretprogram használati utasítását az nhf kiírása tartalmazza, lásd
a DP-honlapon.

A futási időket általunk viszonylag gyorsnak vélt algoritmussal mértük,
feladatonként 2 perces időkorláttal, egy alábbi tipusú gépen:
  model name  : Intel(R) Core(TM) i5-3230M CPU @ 2.60GHz

A feladatot megoldó programot a ksatrak.pl állomány és a tests mappa mellé
(ezekkel azonos mappába) kell elhelyezni satrak.pl néven.

Ugyanebben a mappában kell kiadni az alábbi parancsot a mérés futtatásához:

  sicstus --nologo --noinfo -l ksatrak.pl --goal 'teljes_teszt(120),halt.'

Itt a teljes_teszt(Timeout) eljárás a 'tests' mappában levő összes
"testXXXd.txt" tesztállományra

- lefuttatja a tesztet Timeout másodperces időkorláttal,

- ellenőrzi, hogy a ./tests/testXXXs.txt állományban megadott
  megoldáshalmazt kapta-e,

- megoldáslista formájában valamint olvasható alakban is kiírja az
  eredményt a tests_out_pl mappa testXXXt.txt nevű állományába.

Az állománynevekben az XXX szám tetszőleges hosszúságú lehet.

A tests mappában 36 tesztesetet adunk közre. A beadáskor ezek egy
részhalmazával teszteljük a programokat, ez a részhalmaz a következő:

tests könyvtárbeli név     teszteléskor megjelenő név

test07d.txt                1. teszteset
test10d.txt                2. teszteset
test17d.txt                3. teszteset
test18d.txt                4. teszteset
test19d.txt                5. teszteset
test20d.txt                6. teszteset
test23d.txt                7. teszteset
test24d.txt                8. teszteset
test28d.txt                9. teszteset
test29d.txt               10. teszteset
		    

