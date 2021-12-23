package freskog.aoc.day19

import zio.test._

import Day19Solution._

object Day19SolutionSpec extends DefaultRunnableSpec {

  val spec =
    suite("Day19")(
      suite("Part1")(
        suite("determine scanner pos in 2d")(
          test("Given sample beacons, posX for Scanner 2 = 5") {
            val initialPos = Pos(0,0,0)
            val initialBeacons = LazyList(Pos(0, 2, 0), Pos(4, 1, 0), Pos(3, 3, 0))
            val nextBeacons = LazyList(Pos(-1, -1, 0), Pos(-5, 0, 0), Pos(-2, 1, 0))
            val s0 = Scanner(initialPos, initialBeacons, initialBeacons.toSet[Pos])
            val s1 = Scanner(Pos(5, 2, 0), nextBeacons, initialBeacons.toSet[Pos])
            assertTrue(s0.nextScanner(nextBeacons, 3).get == s1)
          },
          suite("Rotations in 3d are correct")(
            test("Given sample beacon location, can generate 24 orientations")(
              assertTrue(allRotations.map(_(Pos(5, 6, -4))).toSet.size == 24)
            ),
            test("Given sample beacon location, can generate correct orientations") {
              val rotations = allRotations.map(_(Pos(5, 6, -4))).toSet
              assertTrue(rotations.contains(Pos(-5, 4, -6))) &&
              assertTrue(rotations.contains(Pos(-4, -6, 5))) &&
              assertTrue(rotations.contains(Pos(4, 6, 5))) &&
              assertTrue(rotations.contains(Pos(-6, -4, -5)))
            },
            test("Can create scanner from rotated input") {
              val s0 = Scanner(Pos(0,0,0), Pos.from(scanner0), Pos.from(scanner0).toSet[Pos])
              val s1 = s0.nextScanner(Pos.from(scanner1), 12)
              val expected = Pos.from(scanner0and1).toSet[Pos]
              val actual = s0.sharedBeacons(s1.get)
              assertTrue(actual == expected) &&
              assertTrue(s1.get.pos == Pos(68,-1246,-43))
            },
            test("Can create transitive scanner4 from scanner1 created from scanner0") {
              val s0 = Scanner(Pos(0,0,0), Pos.from(scanner0), Pos.from(scanner0).toSet[Pos])
              val Some((s1,s4)) = for {
                s1 <- s0.nextScanner(Pos.from(scanner1), 12)
                s4 <- s1.nextScanner(Pos.from(scanner4), 12)
              } yield (s1,s4)
              val expected = Pos.from(scanner1and4).toSet[Pos]
              val actual = s1.sharedBeacons(s4)
              assertTrue(actual == expected) &&
                assertTrue(Pos(-20,-1133,1061) == s4.pos)
            },
            test("Find all scanners") {
              val s0 = Scanner(Pos(0,0,0), Pos.from(scanner0), Pos.from(scanner0).toSet[Pos])
              val actual =
                findAllScanners(
                  LazyList(s0),
                  Pos.from(scanner1) +: Pos.from(scanner2) +: Pos.from(scanner3) +: Pos.from(scanner4) +: Nil
                )
              val scannerPoses = actual.map(_.pos)
              assertTrue(scannerPoses.contains(Pos(0,0,0))) &&
              assertTrue(scannerPoses.contains(Pos(68,-1246,-43))) &&
              assertTrue(scannerPoses.contains(Pos(1105,-1205,1229))) &&
              assertTrue(scannerPoses.contains(Pos(-92,-2380,-20))) &&
              assertTrue(scannerPoses.contains(Pos(-20,-1133,1061)))
            },
            test("Can find all beacons") {
              val s0 = Scanner(Pos(0,0,0), Pos.from(scanner0), Pos.from(scanner0).toSet[Pos])
              val actual =
                findAllScanners(
                  LazyList(s0),
                  Pos.from(scanner1) +: Pos.from(scanner2) +: Pos.from(scanner3) +: Pos.from(scanner4) +: Nil
                )
              val uniqueBeacons = actual.foldLeft(Set.empty[Pos])(_ ++ _.beacons)
              val expected = Pos.from(allUniqueBeacons).toSet[Pos]
              assertTrue(uniqueBeacons == expected) &&
              assertTrue(uniqueBeacons.size == 79)
            }
          )
        )
      ),
      suite("Part2")(
        test("longest manhattan distance between two scanners") {
          val s0 = Scanner(Pos(0,0,0), Pos.from(scanner0), Pos.from(scanner0).toSet[Pos])
          val scanners =
            findAllScanners(
              LazyList(s0),
              Pos.from(scanner1) +: Pos.from(scanner2) +: Pos.from(scanner3) +: Pos.from(scanner4) +: Nil
            )
          assertTrue(longestDist(scanners) == 3621)
        }
      )

    )

  def scanner0 =
    """404,-588,-901
      |528,-643,409
      |-838,591,734
      |390,-675,-793
      |-537,-823,-458
      |-485,-357,347
      |-345,-311,381
      |-661,-816,-575
      |-876,649,763
      |-618,-824,-621
      |553,345,-567
      |474,580,667
      |-447,-329,318
      |-584,868,-557
      |544,-627,-890
      |564,392,-477
      |455,729,728
      |-892,524,684
      |-689,845,-530
      |423,-701,434
      |7,-33,-71
      |630,319,-379
      |443,580,662
      |-789,900,-551
      |459,-707,401""".stripMargin

  def scanner1 =
    """686,422,578
      |605,423,415
      |515,917,-361
      |-336,658,858
      |95,138,22
      |-476,619,847
      |-340,-569,-846
      |567,-361,727
      |-460,603,-452
      |669,-402,600
      |729,430,532
      |-500,-761,534
      |-322,571,750
      |-466,-666,-811
      |-429,-592,574
      |-355,545,-477
      |703,-491,-529
      |-328,-685,520
      |413,935,-424
      |-391,539,-444
      |586,-435,557
      |-364,-763,-893
      |807,-499,-711
      |755,-354,-619
      |553,889,-390""".stripMargin

  def scanner0and1 = """-618,-824,-621
                       |-537,-823,-458
                       |-447,-329,318
                       |404,-588,-901
                       |544,-627,-890
                       |528,-643,409
                       |-661,-816,-575
                       |390,-675,-793
                       |423,-701,434
                       |-345,-311,381
                       |459,-707,401
                       |-485,-357,347""".stripMargin

  def scanner2 = """649,640,665
                   |682,-795,504
                   |-784,533,-524
                   |-644,584,-595
                   |-588,-843,648
                   |-30,6,44
                   |-674,560,763
                   |500,723,-460
                   |609,671,-379
                   |-555,-800,653
                   |-675,-892,-343
                   |697,-426,-610
                   |578,704,681
                   |493,664,-388
                   |-671,-858,530
                   |-667,343,800
                   |571,-461,-707
                   |-138,-166,112
                   |-889,563,-600
                   |646,-828,498
                   |640,759,510
                   |-630,509,768
                   |-681,-892,-333
                   |673,-379,-804
                   |-742,-814,-386
                   |577,-820,562
                   |""".stripMargin

  def scanner3 = """-589,542,597
                   |605,-692,669
                   |-500,565,-823
                   |-660,373,557
                   |-458,-679,-417
                   |-488,449,543
                   |-626,468,-788
                   |338,-750,-386
                   |528,-832,-391
                   |562,-778,733
                   |-938,-730,414
                   |543,643,-506
                   |-524,371,-870
                   |407,773,750
                   |-104,29,83
                   |378,-903,-323
                   |-778,-728,485
                   |426,699,580
                   |-438,-605,-362
                   |-469,-447,-387
                   |509,732,623
                   |647,635,-688
                   |-868,-804,481
                   |614,-800,639
                   |595,780,-596""".stripMargin

  def scanner4 = """727,592,562
                   |-293,-554,779
                   |441,611,-461
                   |-714,465,-776
                   |-743,427,-804
                   |-660,-479,-426
                   |832,-632,460
                   |927,-485,-438
                   |408,393,-506
                   |466,436,-512
                   |110,16,151
                   |-258,-428,682
                   |-393,719,612
                   |-211,-452,876
                   |808,-476,-593
                   |-575,615,604
                   |-485,667,467
                   |-680,325,-822
                   |-627,-443,-432
                   |872,-547,-609
                   |833,512,582
                   |807,604,487
                   |839,-516,451
                   |891,-625,532
                   |-652,-548,-490
                   |30,-46,-14
                   |""".stripMargin

  def scanner1and4 = """459,-707,401
                       |-739,-1745,668
                       |-485,-357,347
                       |432,-2009,850
                       |528,-643,409
                       |423,-701,434
                       |-345,-311,381
                       |408,-1815,803
                       |534,-1912,768
                       |-687,-1600,576
                       |-447,-329,318
                       |-635,-1737,486""".stripMargin

  def allUniqueBeacons = """-892,524,684
                           |-876,649,763
                           |-838,591,734
                           |-789,900,-551
                           |-739,-1745,668
                           |-706,-3180,-659
                           |-697,-3072,-689
                           |-689,845,-530
                           |-687,-1600,576
                           |-661,-816,-575
                           |-654,-3158,-753
                           |-635,-1737,486
                           |-631,-672,1502
                           |-624,-1620,1868
                           |-620,-3212,371
                           |-618,-824,-621
                           |-612,-1695,1788
                           |-601,-1648,-643
                           |-584,868,-557
                           |-537,-823,-458
                           |-532,-1715,1894
                           |-518,-1681,-600
                           |-499,-1607,-770
                           |-485,-357,347
                           |-470,-3283,303
                           |-456,-621,1527
                           |-447,-329,318
                           |-430,-3130,366
                           |-413,-627,1469
                           |-345,-311,381
                           |-36,-1284,1171
                           |-27,-1108,-65
                           |7,-33,-71
                           |12,-2351,-103
                           |26,-1119,1091
                           |346,-2985,342
                           |366,-3059,397
                           |377,-2827,367
                           |390,-675,-793
                           |396,-1931,-563
                           |404,-588,-901
                           |408,-1815,803
                           |423,-701,434
                           |432,-2009,850
                           |443,580,662
                           |455,729,728
                           |456,-540,1869
                           |459,-707,401
                           |465,-695,1988
                           |474,580,667
                           |496,-1584,1900
                           |497,-1838,-617
                           |527,-524,1933
                           |528,-643,409
                           |534,-1912,768
                           |544,-627,-890
                           |553,345,-567
                           |564,392,-477
                           |568,-2007,-577
                           |605,-1665,1952
                           |612,-1593,1893
                           |630,319,-379
                           |686,-3108,-505
                           |776,-3184,-501
                           |846,-3110,-434
                           |1135,-1161,1235
                           |1243,-1093,1063
                           |1660,-552,429
                           |1693,-557,386
                           |1735,-437,1738
                           |1749,-1800,1813
                           |1772,-405,1572
                           |1776,-675,371
                           |1779,-442,1789
                           |1780,-1548,337
                           |1786,-1538,337
                           |1847,-1591,415
                           |1889,-1729,1762
                           |1994,-1805,1792""".stripMargin
}
