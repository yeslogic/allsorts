mod common;
mod shape;

#[cfg(test)]
mod syriac_tests {
    use crate::common;
    use crate::shape;

    use allsorts::binary::read::ReadScope;
    use allsorts::gsub::RawGlyph;
    use allsorts::scripts::syriac::gsub_apply_syriac;
    use allsorts::tables::cmap::CmapSubtable;
    use allsorts::tables::OpenTypeFont;
    use allsorts::tag;
    use allsorts::Font;
    use std::rc::Rc;

    #[test]
    fn no_gsub_change() {
        // Each test causes the Syriac shaper to pass through the `SyriacGlyph`s unchanged
        test(vec![
            (
                "tests/fonts/noto/NotoSansSyriacEastern-Regular.ttf",
                "\u{700}\u{700}\u{700}",
                vec![125, 125, 125],
            ),
            (
                "tests/fonts/noto/NotoSansSyriacEastern-Regular.ttf",
                "\u{700}\u{700}\u{701}",
                vec![125, 125, 126],
            ),
            (
                "tests/fonts/noto/NotoSansSyriacEastern-Regular.ttf",
                "\u{700}\u{700}\u{702}",
                vec![125, 125, 127],
            ),
            (
                "tests/fonts/noto/NotoSansSyriacEastern-Regular.ttf",
                "\u{700}\u{700}\u{703}",
                vec![125, 125, 128],
            ),
        ])
    }

    #[test]
    fn ccmp() {
        // Each test contains at least one glyph that should trigger `GSUB`'s `CCMP` feature
        test(vec![
            (
                "tests/fonts/noto/NotoSansSyriacEastern-Regular.ttf",
                "\u{700}\u{700}\u{732}",
                vec![125, 125, 291, 292],
            ),
            (
                "tests/fonts/noto/NotoSansSyriacEastern-Regular.ttf",
                "\u{700}\u{701}\u{732}",
                vec![125, 126, 291, 292],
            ),
            (
                "tests/fonts/noto/NotoSansSyriacEastern-Regular.ttf",
                "\u{705}\u{700}\u{732}",
                vec![130, 125, 291, 292],
            ),
            (
                "tests/fonts/noto/NotoSansSyriacEastern-Regular.ttf",
                "\u{706}\u{703}\u{732}",
                vec![131, 128, 291, 292],
            ),
        ])
    }

    #[test]
    fn rlig() {
        // Each test contains at least one glyph that should trigger `GSUB`'s `RLIG` feature
        test(vec![
            (
                "tests/fonts/syriac/SyrCOMBatnan.otf",
                "\u{710}\u{720}\u{712}",
                vec![429, 362],
            ),
            (
                "tests/fonts/syriac/SyrCOMBatnan.otf",
                "\u{710}\u{720}\u{713}",
                vec![429, 365],
            ),
            (
                "tests/fonts/syriac/SyrCOMBatnan.otf",
                "\u{710}\u{720}\u{714}",
                vec![429, 368],
            ),
            (
                "tests/fonts/syriac/SyrCOMBatnan.otf",
                "\u{710}\u{720}\u{715}",
                vec![429, 369],
            ),
        ])
    }

    #[test]
    fn calt() {
        // Each test contains at least one glyph that should trigger `GSUB`'s `CALT` feature
        test(vec![
            (
                "tests/fonts/syriac/SyrCOMAntioch.otf",
                "\u{728}\u{71d}\u{722}",
                vec![317, 383, 237, 398],
            ),
            (
                "tests/fonts/syriac/SyrCOMEdessa.otf",
                "\u{71b}\u{710}\u{700}",
                vec![210, 88, 190, 111],
            ),
            (
                "tests/fonts/syriac/SyrCOMNisibin.otf",
                "\u{71b}\u{710}\u{701}",
                vec![377, 255, 357, 279],
            ),
            (
                "tests/fonts/syriac/SyrCOMNisibin.otf",
                "\u{71b}\u{710}\u{704}",
                vec![377, 255, 357, 282],
            ),
        ])
    }

    #[test]
    fn liga() {
        // Each test contains at least one glyph that should trigger `GSUB`'s `LIGA` feature
        test(vec![
            (
                "tests/fonts/syriac/SyrCOMAdiabene.otf",
                "\u{700}\u{717}\u{71d}",
                vec![278, 429],
            ),
            (
                "tests/fonts/syriac/SyrCOMAdiabene.otf",
                "\u{701}\u{717}\u{71d}",
                vec![279, 429],
            ),
            (
                "tests/fonts/syriac/SyrCOMAdiabene.otf",
                "\u{712}\u{72c}\u{710}",
                vec![360, 428],
            ),
            (
                "tests/fonts/syriac/SyrCOMAdiabene.otf",
                "\u{717}\u{743}\u{71d}",
                vec![429, 341],
            ),
            // We current don't apply `LIGA` across space boundaries
            (
                "tests/fonts/syriac/SyrCOMMalankara.otf",
                "\u{720}\u{20}\u{710}",
                vec![309, 3, 358],
            ),
        ])
    }

    trait IIMF {
        fn add(&mut self, input_shorthand: &str, expected_shorthand: &str);
    }

    impl IIMF for Vec<(String, Vec<u16>)> {
        fn add(&mut self, input_shorthand: &str, expected_shorthand: &str) {
            // To aid reading ISOL, INIT, MED*, and FIN* tests, we use ASCII box drawing characters
            // to represent Alaph (A), non-joining (┃), right-joining (┗), right-joining Dalath
            // Rish (╚), and dual-joining (┻) Syriac glyphs
            //
            // The characters above may look like underscores in some environments, so make sure
            // your `LC_CTYPE` environment variable is set accordingly to make them differ :)

            const ALAPH: char = '\u{710}';
            const NON_JOINING: char = '\u{700}';
            const RIGHT_JOINING: char = '\u{717}';
            const RIGHT_JOINING_DALATH_RISH: char = '\u{715}';
            const DUAL_JOINING: char = '\u{712}';

            let input: Vec<char> = input_shorthand
                .chars()
                .rev()
                .map(|c| match c {
                    'A' => ALAPH,
                    '┃' => NON_JOINING,
                    '┗' => RIGHT_JOINING,
                    '╚' => RIGHT_JOINING_DALATH_RISH,
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
                        ALAPH => 8,
                        NON_JOINING => 125,
                        RIGHT_JOINING => 30,
                        RIGHT_JOINING_DALATH_RISH => 26,
                        DUAL_JOINING => 14,
                        _ => invalid_expected(t),
                    },
                    "INIT" => match input[i] {
                        DUAL_JOINING => 17,
                        _ => invalid_expected(t),
                    },
                    "MEDI" => match input[i] {
                        DUAL_JOINING => 16,
                        _ => invalid_expected(t),
                    },
                    "MED2" => match input[i] {
                        ALAPH => 12,
                        _ => invalid_expected(t),
                    },
                    "FINA" => match input[i] {
                        RIGHT_JOINING => 31,
                        RIGHT_JOINING_DALATH_RISH => 27,
                        DUAL_JOINING => 15,
                        ALAPH => 11,
                        _ => invalid_expected(t),
                    },
                    "FIN2" => match input[i] {
                        ALAPH => 10,
                        _ => invalid_expected(t),
                    },
                    "FIN3" => match input[i] {
                        ALAPH => 9,
                        _ => invalid_expected(t),
                    },
                    _ => invalid_expected(t),
                })
                .into_iter()
                .collect();

            self.push((input.into_iter().collect::<String>(), expected));
        }
    }

    #[test]
    fn isol_init_medi_fina() {
        let mut test_cases: Vec<(String, Vec<u16>)> = vec![];

        // To understand the left hand side vs the right hand side of the `add()`, if a glyph is:
        //   - by itself, it's ISOL
        //   - at the start of a grouping (remember right to left!), it's INIT
        //   - in the middle of a grouping, it's MEDI
        //   - in the middle of a grouping and it's Alaph, it's MED2
        //   - at the end of a grouping, it's FINA
        //   - at the end of a grouping and it's Alaph, it's FIN2
        //   - at the end of a grouping and it's Alaph, and follows Dalath Rish, it's FIN3

        test_cases.add("A", "ISOL");
        test_cases.add("┃", "ISOL");
        test_cases.add("┗", "ISOL");
        test_cases.add("╚", "ISOL");
        test_cases.add("┻", "ISOL");

        test_cases.add("AA", "FIN2 ISOL");
        test_cases.add("A┃", "FIN2 ISOL");
        test_cases.add("A┗", "FIN2 ISOL");
        test_cases.add("A╚", "FIN3 ISOL");
        test_cases.add("A┻", "FINA INIT");

        test_cases.add("┃A", "ISOL ISOL");
        test_cases.add("┃┃", "ISOL ISOL");
        test_cases.add("┃┗", "ISOL ISOL");
        test_cases.add("┃╚", "ISOL ISOL");
        test_cases.add("┃┻", "ISOL ISOL");

        test_cases.add("┗A", "ISOL ISOL");
        test_cases.add("┗┃", "ISOL ISOL");
        test_cases.add("┗┗", "ISOL ISOL");
        test_cases.add("┗╚", "ISOL ISOL");
        test_cases.add("┗┻", "FINA INIT");

        test_cases.add("╚A", "ISOL ISOL");
        test_cases.add("╚┃", "ISOL ISOL");
        test_cases.add("╚┗", "ISOL ISOL");
        test_cases.add("╚╚", "ISOL ISOL");
        test_cases.add("╚┻", "FINA INIT");

        test_cases.add("┻A", "ISOL ISOL");
        test_cases.add("┻┃", "ISOL ISOL");
        test_cases.add("┻┗", "ISOL ISOL");
        test_cases.add("┻╚", "ISOL ISOL");
        test_cases.add("┻┻", "FINA INIT");

        test_cases.add("AAA", "FIN2 ISOL ISOL");
        test_cases.add("AA┃", "FIN2 ISOL ISOL");
        test_cases.add("AA┗", "FIN2 ISOL ISOL");
        test_cases.add("AA╚", "FIN2 ISOL ISOL");
        test_cases.add("AA┻", "FIN2 MED2 INIT");
        test_cases.add("A┃A", "FIN2 ISOL ISOL");
        test_cases.add("A┃┃", "FIN2 ISOL ISOL");
        test_cases.add("A┃┗", "FIN2 ISOL ISOL");
        test_cases.add("A┃╚", "FIN2 ISOL ISOL");
        test_cases.add("A┃┻", "FIN2 ISOL ISOL");
        test_cases.add("A┗A", "FIN2 ISOL ISOL");
        test_cases.add("A┗┃", "FIN2 ISOL ISOL");
        test_cases.add("A┗┗", "FIN2 ISOL ISOL");
        test_cases.add("A┗╚", "FIN2 ISOL ISOL");
        test_cases.add("A┗┻", "FIN2 FINA INIT");
        test_cases.add("A╚A", "FIN3 ISOL ISOL");
        test_cases.add("A╚┃", "FIN3 ISOL ISOL");
        test_cases.add("A╚┗", "FIN3 ISOL ISOL");
        test_cases.add("A╚╚", "FIN3 ISOL ISOL");
        test_cases.add("A╚┻", "FIN3 FINA INIT");
        test_cases.add("A┻A", "FINA INIT ISOL");
        test_cases.add("A┻┃", "FINA INIT ISOL");
        test_cases.add("A┻┗", "FINA INIT ISOL");
        test_cases.add("A┻╚", "FINA INIT ISOL");
        test_cases.add("A┻┻", "FINA MEDI INIT");

        test_cases.add("┃AA", "ISOL FIN2 ISOL");
        test_cases.add("┃A┃", "ISOL FIN2 ISOL");
        test_cases.add("┃A┗", "ISOL FIN2 ISOL");
        test_cases.add("┃A╚", "ISOL FIN3 ISOL");
        test_cases.add("┃A┻", "ISOL FINA INIT");
        test_cases.add("┃┃A", "ISOL ISOL ISOL");
        test_cases.add("┃┃┃", "ISOL ISOL ISOL");
        test_cases.add("┃┃┗", "ISOL ISOL ISOL");
        test_cases.add("┃┃╚", "ISOL ISOL ISOL");
        test_cases.add("┃┃┻", "ISOL ISOL ISOL");
        test_cases.add("┃┗A", "ISOL ISOL ISOL");
        test_cases.add("┃┗┃", "ISOL ISOL ISOL");
        test_cases.add("┃┗┗", "ISOL ISOL ISOL");
        test_cases.add("┃┗╚", "ISOL ISOL ISOL");
        test_cases.add("┃┗┻", "ISOL FINA INIT");
        test_cases.add("┃╚A", "ISOL ISOL ISOL");
        test_cases.add("┃╚┃", "ISOL ISOL ISOL");
        test_cases.add("┃╚┗", "ISOL ISOL ISOL");
        test_cases.add("┃╚╚", "ISOL ISOL ISOL");
        test_cases.add("┃╚┻", "ISOL FINA INIT");
        test_cases.add("┃┻A", "ISOL ISOL ISOL");
        test_cases.add("┃┻┃", "ISOL ISOL ISOL");
        test_cases.add("┃┻┗", "ISOL ISOL ISOL");
        test_cases.add("┃┻╚", "ISOL ISOL ISOL");
        test_cases.add("┃┻┻", "ISOL FINA INIT");

        test_cases.add("┗AA", "ISOL ISOL ISOL");
        test_cases.add("┗A┃", "ISOL ISOL ISOL");
        test_cases.add("┗A┗", "ISOL ISOL ISOL");
        test_cases.add("┗A╚", "ISOL ISOL ISOL");
        test_cases.add("┗A┻", "ISOL MED2 INIT");
        test_cases.add("┗┃A", "ISOL ISOL ISOL");
        test_cases.add("┗┃┃", "ISOL ISOL ISOL");
        test_cases.add("┗┃┗", "ISOL ISOL ISOL");
        test_cases.add("┗┃╚", "ISOL ISOL ISOL");
        test_cases.add("┗┃┻", "ISOL ISOL ISOL");
        test_cases.add("┗┗A", "ISOL ISOL ISOL");
        test_cases.add("┗┗┃", "ISOL ISOL ISOL");
        test_cases.add("┗┗┗", "ISOL ISOL ISOL");
        test_cases.add("┗┗╚", "ISOL ISOL ISOL");
        test_cases.add("┗┗┻", "ISOL FINA INIT");
        test_cases.add("┗╚A", "ISOL ISOL ISOL");
        test_cases.add("┗╚┃", "ISOL ISOL ISOL");
        test_cases.add("┗╚┗", "ISOL ISOL ISOL");
        test_cases.add("┗╚╚", "ISOL ISOL ISOL");
        test_cases.add("┗╚┻", "ISOL FINA INIT");
        test_cases.add("┗┻A", "FINA INIT ISOL");
        test_cases.add("┗┻┃", "FINA INIT ISOL");
        test_cases.add("┗┻┗", "FINA INIT ISOL");
        test_cases.add("┗┻╚", "FINA INIT ISOL");
        test_cases.add("┗┻┻", "FINA MEDI INIT");

        test_cases.add("╚AA", "ISOL ISOL ISOL");
        test_cases.add("╚A┃", "ISOL ISOL ISOL");
        test_cases.add("╚A┗", "ISOL ISOL ISOL");
        test_cases.add("╚A╚", "ISOL ISOL ISOL");
        test_cases.add("╚A┻", "ISOL MED2 INIT");
        test_cases.add("╚┃A", "ISOL ISOL ISOL");
        test_cases.add("╚┃┃", "ISOL ISOL ISOL");
        test_cases.add("╚┃┗", "ISOL ISOL ISOL");
        test_cases.add("╚┃╚", "ISOL ISOL ISOL");
        test_cases.add("╚┃┻", "ISOL ISOL ISOL");
        test_cases.add("╚┗A", "ISOL ISOL ISOL");
        test_cases.add("╚┗┃", "ISOL ISOL ISOL");
        test_cases.add("╚┗┗", "ISOL ISOL ISOL");
        test_cases.add("╚┗╚", "ISOL ISOL ISOL");
        test_cases.add("╚┗┻", "ISOL FINA INIT");
        test_cases.add("╚╚A", "ISOL ISOL ISOL");
        test_cases.add("╚╚┃", "ISOL ISOL ISOL");
        test_cases.add("╚╚┗", "ISOL ISOL ISOL");
        test_cases.add("╚╚╚", "ISOL ISOL ISOL");
        test_cases.add("╚╚┻", "ISOL FINA INIT");
        test_cases.add("╚┻A", "FINA INIT ISOL");
        test_cases.add("╚┻┃", "FINA INIT ISOL");
        test_cases.add("╚┻┗", "FINA INIT ISOL");
        test_cases.add("╚┻╚", "FINA INIT ISOL");
        test_cases.add("╚┻┻", "FINA MEDI INIT");

        test_cases.add("┻AA", "ISOL ISOL ISOL");
        test_cases.add("┻A┃", "ISOL ISOL ISOL");
        test_cases.add("┻A┗", "ISOL ISOL ISOL");
        test_cases.add("┻A╚", "ISOL ISOL ISOL");
        test_cases.add("┻A┻", "ISOL MED2 INIT");
        test_cases.add("┻┃A", "ISOL ISOL ISOL");
        test_cases.add("┻┃┃", "ISOL ISOL ISOL");
        test_cases.add("┻┃┗", "ISOL ISOL ISOL");
        test_cases.add("┻┃╚", "ISOL ISOL ISOL");
        test_cases.add("┻┃┻", "ISOL ISOL ISOL");
        test_cases.add("┻┗A", "ISOL ISOL ISOL");
        test_cases.add("┻┗┃", "ISOL ISOL ISOL");
        test_cases.add("┻┗┗", "ISOL ISOL ISOL");
        test_cases.add("┻┗╚", "ISOL ISOL ISOL");
        test_cases.add("┻┗┻", "ISOL FINA INIT");
        test_cases.add("┻╚A", "ISOL ISOL ISOL");
        test_cases.add("┻╚┃", "ISOL ISOL ISOL");
        test_cases.add("┻╚┗", "ISOL ISOL ISOL");
        test_cases.add("┻╚╚", "ISOL ISOL ISOL");
        test_cases.add("┻╚┻", "ISOL FINA INIT");
        test_cases.add("┻┻A", "FINA INIT ISOL");
        test_cases.add("┻┻┃", "FINA INIT ISOL");
        test_cases.add("┻┻┗", "FINA INIT ISOL");
        test_cases.add("┻┻╚", "FINA INIT ISOL");
        test_cases.add("┻┻┻", "FINA MEDI INIT");

        test_cases.add(
            "┻┻┗┃╚┻A┃┗┻┻┻A┻╚┻┗A┗┻A╚┻",
            "FINA INIT ISOL ISOL FINA INIT ISOL ISOL FINA MEDI MEDI INIT MED2 INIT FINA INIT ISOL ISOL FINA INIT ISOL FINA INIT",
        );

        test_cases.add(
            "A┻┗┃╚┻A┃┗┻┻┻A┻╚┻┗A┗┻A╚┻",
            "FINA INIT ISOL ISOL FINA INIT ISOL ISOL FINA MEDI MEDI INIT MED2 INIT FINA INIT ISOL ISOL FINA INIT ISOL FINA INIT",
        );

        test_cases.add(
            "A┗┗┃╚┻A┃┗┻┻┻A┻╚┻┗A┗┻A╚┻",
            "FIN2 ISOL ISOL ISOL FINA INIT ISOL ISOL FINA MEDI MEDI INIT MED2 INIT FINA INIT ISOL ISOL FINA INIT ISOL FINA INIT",
        );

        test_cases.add(
            "A╚┗┃╚┻A┃┗┻┻┻A┻╚┻┗A┗┻A╚┻",
            "FIN3 ISOL ISOL ISOL FINA INIT ISOL ISOL FINA MEDI MEDI INIT MED2 INIT FINA INIT ISOL ISOL FINA INIT ISOL FINA INIT",
        );

        test(
            test_cases
                .iter()
                .map(|(input, expected)| {
                    (
                        "tests/fonts/noto/NotoSansSyriacEastern-Regular.ttf",
                        input.as_str(),
                        expected.to_vec(),
                    )
                })
                .collect::<Vec<(&str, &str, Vec<u16>)>>(),
        )
    }

    fn test(test_cases: Vec<(&str, &str, Vec<u16>)>) {
        for (font_path, text, expected) in test_cases {
            let font_contents = common::read_fixture(font_path);
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
                .flat_map(|ch| shape::map_glyph(&cmap_subtable, ch))
                .flatten()
                .collect();

            gsub_apply_syriac(
                &gsub_cache,
                &gsub_cache.layout_table,
                font.gdef_table()
                    .expect("Error getting GDEF table")
                    .as_ref()
                    .map(Rc::as_ref),
                tag::SYRC,
                None,
                None,
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
