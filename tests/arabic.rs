mod shape;

#[cfg(test)]
mod arabic_tests {
    use crate::shape;

    use allsorts::binary::read::ReadScope;
    use allsorts::gsub::RawGlyph;
    use allsorts::scripts::arabic::gsub_apply_arabic;
    use allsorts::tables::cmap::CmapSubtable;
    use allsorts::tables::OpenTypeFont;
    use allsorts::tag;
    use allsorts::Font;
    use alloc::rc::Rc;

    #[test]
    fn no_gsub_change() {
        // Each test causes the Arabic shaper to pass through the `ArabicGlyph`s unchanged
        test(
            None,
            vec![
                (
                    include_bytes!("./fonts/arabic/ae_Arab.ttf"),
                    "\u{622}\u{623}\u{625}",
                    vec![498, 499, 501],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-quran.ttf"),
                    "\u{630}\u{64e}\u{631}\u{6e1}\u{648}\u{8f0}\u{627}",
                    vec![51, 76, 52, 117, 70, 140, 42],
                ),
                (include_bytes!("./fonts/arabic/amiri-quran.ttf"), "\u{622}", vec![37]),
                (include_bytes!("./fonts/arabic/amiri-quran.ttf"), "\u{623}", vec![38]),
            ],
        )
    }

    #[test]
    fn ccmp() {
        // Each test contains at least one glyph that should trigger `GSUB`'s `CCMP` feature
        test(
            None,
            vec![
                (
                    include_bytes!("./fonts/arabic/Scheherazade-Regular.ttf"),
                    "\u{644}\u{631}\u{622}\u{65b}\u{640}",
                    vec![1039, 564, 273, 1095, 1086, 519],
                ),
                (
                    include_bytes!("./fonts/arabic/Scheherazade-Regular.ttf"),
                    "\u{644}\u{623}\u{62e}\u{637}\u{650}\u{641}\u{62e}",
                    vec![1330, 1341, 1087, 956, 832, 1079, 844, 633],
                ),
                (
                    include_bytes!("./fonts/arabic/Scheherazade-Regular.ttf"),
                    "\u{62f}\u{63a}\u{643}\u{625}\u{64c}\u{62e}",
                    vec![298, 994, 861, 524, 1088, 1071, 397],
                ),
                (
                    include_bytes!(i.!("tests/fonts/arabic/Scheherazade-Regular.ttf"),
                    "\u{653}\u{630}\u{62e}",
                    vec![1086, 299, 397],
                ),
            ],
        )
    }

    #[test]
    fn locl_farsi() {
        // Each test contains at least one glyph that should trigger `GSUB`'s `LOCL` feature
        test(
            Some(tag::FAR),
            vec![
                (
                    include_bytes!("./fonts/noto/NotoNaskhArabic-Regular.ttf"),
                    "\u{66b}\u{db}\u{b4}",
                    vec![784],
                ),
                (
                    include_bytes!("./fonts/noto/NotoNaskhArabic-Regular.ttf"),
                    "\u{66c}\u{db}\u{b4}",
                    vec![789],
                ),
                (
                    include_bytes!("./fonts/noto/NotoNaskhArabic-Regular.ttf"),
                    "\u{66b}\u{db}\u{b6}",
                    vec![784],
                ),
                (
                    include_bytes!("./fonts/noto/NotoNaskhArabic-Regular.ttf"),
                    "\u{66c}\u{db}\u{b6}",
                    vec![789],
                ),
            ],
        )
    }

    #[test]
    fn locl_sindhi() {
        // Each test contains at least one glyph that should trigger `GSUB`'s `LOCL` feature
        test(
            Some(tag::SND),
            vec![
                (
                    include_bytes!("./fonts/arabic/amiri-regular.ttf"),
                    "\u{67A}\u{2e}",
                    vec![471, 1914],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-regular.ttf"),
                    "\u{67B}\u{2e}",
                    vec![472, 1914],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-regular.ttf"),
                    "\u{67D}\u{2e}",
                    vec![474, 1914],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-regular.ttf"),
                    "\u{67F}\u{2e}",
                    vec![476, 1914],
                ),
            ],
        )
    }

    #[test]
    fn locl_urdu() {
        // Each test contains at least one glyph that should trigger `GSUB`'s `LOCL` feature
        test(
            Some(tag::URD),
            vec![
                (
                    include_bytes!("./fonts/arabic/amiri-regular.ttf"),
                    "\u{679}\u{2e}",
                    vec![470, 1914],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-regular.ttf"),
                    "\u{686}\u{2e}",
                    vec![483, 1914],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-regular.ttf"),
                    "\u{688}\u{2e}",
                    vec![485, 1914],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-regular.ttf"),
                    "\u{691}\u{2e}",
                    vec![494, 1914],
                ),
            ],
        )
    }

    #[test]
    fn locl() {
        // Each test contains at least one glyph that should trigger `GSUB`'s `LOCL` feature
        test(
            None,
            vec![
                (
                    include_bytes!("./fonts/arabic/amiri-regular.ttf"),
                    "\u{606}\u{2e}",
                    vec![357, 1914],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-regular.ttf"),
                    "\u{607}\u{2e}",
                    vec![358, 1914],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-regular.ttf"),
                    "\u{608}\u{2e}",
                    vec![359, 1914],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-regular.ttf"),
                    "\u{609}\u{2e}",
                    vec![360, 1914],
                ),
            ],
        )
    }

    #[test]
    fn rlig() {
        // Each test contains at least one glyph that should trigger `GSUB`'s `RLIG` feature
        test(
            None,
            vec![
                (
                    include_bytes!("./fonts/arabic/amiri-quran.ttf"),
                    "\u{671}\u{644}\u{633}\u{651}\u{64e}\u{645}\u{64e}\u{640}\u{670}\u{648}",
                    vec![101, 531, 552, 79, 377, 535, 76, 1311, 561],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-quran.ttf"),
                    concat!(
                        "\u{645}\u{651}\u{64f}\u{628}\u{64e}\u{640}\u{670}\u{631}\u{64e}\u{643}",
                        "\u{64e}\u{629}\u{650}\u{650}"
                    ),
                    vec![534, 79, 379, 494, 76, 1311, 541, 76, 1111, 76, 1114, 78, 78],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-quran.ttf"),
                    concat!(
                        "\u{671}\u{644}\u{6e1}\u{623}\u{64e}\u{645}\u{6e1}\u{62b}\u{64e}\u{640}",
                        "\u{670}\u{644}\u{64e}",
                    ),
                    vec![101, 698, 117, 701, 1339, 534, 117, 498, 76, 1311, 530, 76],
                ),
                (
                    include_bytes!("./fonts/arabic/NafeesNastaleeq.ttf"),
                    "\u{647}\u{644}\u{627}",
                    vec![44],
                ),
            ],
        )
    }

    #[test]
    fn calt() {
        // Each test contains at least one glyph that should trigger `GSUB`'s `CALT` feature
        test(
            None,
            vec![
                (
                    include_bytes!("./fonts/arabic/amiri-quran.ttf"),
                    concat!(
                        "\u{627}\u{644}\u{623}\u{64e}\u{628}\u{652}\u{62c}\u{64e}\u{62f}\u{650}",
                        "\u{64a}\u{64e}\u{651}\u{629}",
                    ),
                    vec![
                        42, 698, 701, 1339, 991, 80, 942, 76, 503, 78, 482, 76, 79, 524,
                    ],
                ),
                (
                    include_bytes!("./fonts/arabic/NafeesNastaleeq.ttf"),
                    "\u{645}\u{631}",
                    vec![392, 73],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-quran.ttf"),
                    "\u{64a}\u{62a}\u{645}\u{64a}\u{632}",
                    vec![883, 898, 903, 771, 780],
                ),
                (
                    include_bytes!("./fonts/arabic/amiri-quran.ttf"),
                    "\u{627}\u{644}\u{623}\u{645}\u{64a}\u{631}\u{64a}\u{629}",
                    vec![42, 698, 701, 534, 771, 781, 482, 524],
                ),
            ],
        )
    }

    #[test]
    fn liga() {
        // Each test contains at least one glyph that should trigger `GSUB`'s `LIGA` feature
        test(
            None,
            vec![
                (
                    include_bytes!("./fonts/arabic/ae_Arab.ttf"),
                    "\u{644}\u{627}",
                    vec![932],
                ),
                (
                    include_bytes!("./fonts/arabic/ae_Arab.ttf"),
                    "\u{647}\u{644}\u{627}",
                    vec![916, 933],
                ),
                (
                    include_bytes!("./fonts/arabic/ae_Arab.ttf"),
                    "\u{627}\u{644}\u{623}\u{64e}\u{631}\u{652}\u{636}\u{650}",
                    vec![503, 928, 537, 513, 541, 518, 539],
                ),
                (
                    include_bytes!("./fonts/arabic/ae_Arab.ttf"),
                    "\u{623}\u{64e}\u{648}\u{651}\u{64e}\u{644}\u{627}\u{64b}",
                    vec![499, 537, 531, 540, 537, 932, 534],
                ),
            ],
        )
    }

    #[test]
    fn mset() {
        // Each test contains at least one glyph that should trigger `GSUB`'s `MSET` feature
        test(
            None,
            vec![
                (
                    include_bytes!("./fonts/arabic/KacstBook.ttf"),
                    "\u{648}\u{64e}\u{643}\u{64e}\u{627}\u{646}\u{64e}",
                    vec![441, 259, 423, 322, 346, 433, 322],
                ),
                (
                    include_bytes!("./fonts/arabic/KacstBook.ttf"),
                    "\u{627}\u{644}\u{623}\u{64e}\u{631}\u{652}\u{636}\u{650}",
                    vec![345, 451, 200, 377, 261, 393, 326],
                ),
                (
                    include_bytes!("./fonts/arabic/KacstBook.ttf"),
                    "\u{62c}\u{64e}\u{645}\u{650}\u{64a}\u{639}\u{627}\u{64b}",
                    vec![363, 259, 432, 265, 448, 408, 217],
                ),
                (
                    include_bytes!("./fonts/arabic/KacstBook.ttf"),
                    concat!(
                        "\u{64a}\u{64e}\u{62a}\u{64e}\u{643}\u{64e}\u{644}\u{651}\u{64e}\u{645}",
                        "\u{64f}\u{648}\u{646}\u{64e}",
                    ),
                    vec![
                        447, 259, 356, 259, 424, 322, 428, 307, 432, 260, 442, 433, 322,
                    ],
                ),
            ],
        )
    }

    trait IIMF {
        fn add(&mut self, input_shorthand: &str, expected_shorthand: &str);
    }

    impl IIMF for Vec<(String, Vec<u16>)> {
        fn add(&mut self, input_shorthand: &str, expected_shorthand: &str) {
            // To aid reading ISOL, INIT, MEDI, and FINA tests, we use ASCII box drawing characters
            // to represent non-joining (┃), right-joining (┗), and dual-joining (┻) Arabic glyphs
            //
            // the characters above may look like underscores in some environments, so make sure
            // your `LC_CTYPE` environment variable is set accordingly to make them differ :)

            const NON_JOINING: char = '\u{621}';
            const DUAL_JOINING: char = '\u{634}';
            const RIGHT_JOINING: char = '\u{630}';

            let input: Vec<char> = input_shorthand
                .chars()
                .rev()
                .map(|c| match c {
                    '┃' => NON_JOINING,
                    '┗' => RIGHT_JOINING,
                    '┻' => DUAL_JOINING,
                    _ => panic!("Invalid input parameter '{}'", c),
                })
                .into_iter()
                .collect();

            let invalid_expected = |t| panic!("Invalid expected parameter '{}'", t);

            let expected = expected_shorthand
                .split_whitespace()
                .rev()
                .enumerate()
                .map(|(i, t)| match t {
                    "ISOL" => match input[i] {
                        NON_JOINING => 497,
                        DUAL_JOINING => 516,
                        RIGHT_JOINING => 512,
                        _ => invalid_expected(t),
                    },
                    "INIT" => match input[i] {
                        DUAL_JOINING => 864,
                        _ => invalid_expected(t),
                    },
                    "MEDI" => match input[i] {
                        DUAL_JOINING => 865,
                        _ => invalid_expected(t),
                    },
                    "FINA" => match input[i] {
                        DUAL_JOINING => 863,
                        RIGHT_JOINING => 853,
                        _ => invalid_expected(t),
                    },
                    _ => invalid_expected(t),
                })
                .into_iter()
                .collect();

            self.push((input.into_iter().collect::<String>(), expected))
        }
    }

    #[test]
    fn isol_init_medi_fina() {
        let mut test_cases: Vec<(String, Vec<u16>)> = vec![];

        // To understand the left hand side vs the right hand side of the `add()`:
        //   - if a glyph is by itself, it should match ISOL
        //   - if a glyph is at the start of a grouping (remember right to left!), it's INIT
        //   - if a glyph is in the middle of a grouping, it's MEDI
        //   - if a glyph is at the end of a grouping, it's FINA

        test_cases.add("┃", "ISOL");
        test_cases.add("┗", "ISOL");
        test_cases.add("┻", "ISOL");

        test_cases.add("┃┃", "ISOL ISOL");
        test_cases.add("┃┗", "ISOL ISOL");
        test_cases.add("┃┻", "ISOL ISOL");

        test_cases.add("┗┃", "ISOL ISOL");
        test_cases.add("┗┗", "ISOL ISOL");
        test_cases.add("┗┻", "FINA INIT");
        test_cases.add("┻┃", "ISOL ISOL");
        test_cases.add("┻┗", "ISOL ISOL");
        test_cases.add("┻┻", "FINA INIT");

        test_cases.add("┃┃┃", "ISOL ISOL ISOL");
        test_cases.add("┃┃┗", "ISOL ISOL ISOL");
        test_cases.add("┃┃┻", "ISOL ISOL ISOL");
        test_cases.add("┃┗┃", "ISOL ISOL ISOL");
        test_cases.add("┃┗┗", "ISOL ISOL ISOL");
        test_cases.add("┃┗┻", "ISOL FINA INIT");
        test_cases.add("┃┻┃", "ISOL ISOL ISOL");
        test_cases.add("┃┻┗", "ISOL ISOL ISOL");
        test_cases.add("┃┻┻", "ISOL FINA INIT");

        test_cases.add("┗┃┃", "ISOL ISOL ISOL");
        test_cases.add("┗┃┗", "ISOL ISOL ISOL");
        test_cases.add("┗┃┻", "ISOL ISOL ISOL");
        test_cases.add("┗┗┃", "ISOL ISOL ISOL");
        test_cases.add("┗┗┗", "ISOL ISOL ISOL");
        test_cases.add("┗┗┻", "ISOL FINA INIT");
        test_cases.add("┗┻┃", "FINA INIT ISOL");
        test_cases.add("┗┻┗", "FINA INIT ISOL");
        test_cases.add("┗┻┻", "FINA MEDI INIT");
        test_cases.add("┻┃┃", "ISOL ISOL ISOL");
        test_cases.add("┻┃┗", "ISOL ISOL ISOL");
        test_cases.add("┻┃┻", "ISOL ISOL ISOL");
        test_cases.add("┻┗┃", "ISOL ISOL ISOL");
        test_cases.add("┻┗┗", "ISOL ISOL ISOL");
        test_cases.add("┻┗┻", "ISOL FINA INIT");
        test_cases.add("┻┻┃", "FINA INIT ISOL");
        test_cases.add("┻┻┗", "FINA INIT ISOL");
        test_cases.add("┻┻┻", "FINA MEDI INIT");

        test_cases.add(
            "┻┗┻┃┻┗┃┃┗┗┻┻┻┻┃┗",
            "ISOL FINA INIT ISOL ISOL ISOL ISOL ISOL ISOL FINA MEDI MEDI MEDI INIT ISOL ISOL",
        );

        test(
            None,
            test_cases
                .iter()
                .map(|(input, expected)| {
                    (
                        include_bytes!("./fonts/arabic/ae_Arab.ttf"),
                        input.as_str(),
                        expected.to_vec(),
                    )
                })
                .collect::<Vec<(&str, &str, Vec<u16>)>>(),
        )
    }

    fn test(lang_tag: Option<u32>, test_cases: Vec<(&'static [u8], &str, Vec<u16>)>) {
        for (font_path, text, expected) in test_cases {
            let font_contents = common::include_bytes!(font_path);
            let opentype_file = ReadScope::new(&font_contents)
                .read::<OpenTypeFont>()
                .unwrap();

            let font_table_provider = opentype_file
                .table_provider(0)
                .expect("Error getting font file");

            let mut font = Font::new(Box::new(font_table_provider))
                .expect("Error getting font data")
                .expect("Missing font tables");

            let gsub_cache = font
                .gsub_cache()
                .expect("Error getting GSUB cache")
                .expect("Missing GSUB table");

            let cmap_subtable_data = font.cmap_subtable_data().to_vec();

            let cmap_subtable = ReadScope::new(&cmap_subtable_data)
                .read::<CmapSubtable<'_>>()
                .expect("Error getting CMAP subtable");

            let mut raw_glyphs: Vec<RawGlyph<()>> = text
                .chars()
                .map(|ch| shape::map_glyph(&cmap_subtable, ch))
                .flatten()
                .flatten()
                .collect();

            gsub_apply_arabic(
                &gsub_cache,
                &gsub_cache.layout_table,
                font.gdef_table()
                    .expect("Error getting GDEF table")
                    .as_ref()
                    .map(Rc::as_ref),
                tag::ARAB,
                lang_tag,
                &mut raw_glyphs,
            )
            .unwrap();

            assert_eq!(
                raw_glyphs
                    .iter()
                    .map(|g| g.glyph_index)
                    .collect::<Vec<u16>>(),
                expected,
            )
        }
    }
}
