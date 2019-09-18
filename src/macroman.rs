#![deny(missing_docs)]

/// Returns `true` if the supplied `char` exists in the Mac OS Roman character encoding.
///
/// https://en.wikipedia.org/wiki/Mac_OS_Roman
pub fn is_macroman(chr: char) -> bool {
    char_to_macroman(chr).is_some()
}

/// Converts a `char` to its Mac OS Roman character encoding.
///
/// Returns `None` if the character is not part of the Mac OS Roman character set.
#[rustfmt::skip]
pub fn char_to_macroman(chr: char) -> Option<u8> {
    if (chr as u32) < 0x7F {
        Some(chr as u8)
    } else {
        match chr {
            'Ä' => Some(128), // A dieresis
            'Å' => Some(129), // A ring
            'Ç' => Some(130), // C cedilla
            'É' => Some(131), // E acute
            'Ñ' => Some(132), // N tilde
            'Ö' => Some(133), // O dieresis
            'Ü' => Some(134), // U dieresis
            'á' => Some(135), // a acute
            'à' => Some(136), // a grave
            'â' => Some(137), // a circumflex
            'ä' => Some(138), // a dieresis
            'ã' => Some(139), // a tilde
            'å' => Some(140), // a ring
            'ç' => Some(141), // c cedilla
            'é' => Some(142), // e acute
            'è' => Some(143), // e grave
            'ê' => Some(144), // e circumflex
            'ë' => Some(145), // e dieresis
            'í' => Some(146), // i acute
            'ì' => Some(147), // i grave
            'î' => Some(148), // i circumflex
            'ï' => Some(149), // i dieresis
            'ñ' => Some(150), // n tilde
            'ó' => Some(151), // o acute
            'ò' => Some(152), // o grave
            'ô' => Some(153), // o circumflex
            'ö' => Some(154), // o dieresis
            'õ' => Some(155), // o tilde
            'ú' => Some(156), // u acute
            'ù' => Some(157), // u grave
            'û' => Some(158), // u circumflex
            'ü' => Some(159), // u dieresis
            '†' => Some(160), // dagger
            '°' => Some(161), // degree
            '¢' => Some(162), // cent
            '£' => Some(163), // sterling
            '§' => Some(164), // section
            '•' => Some(165), // bullet
            '¶' => Some(166), // paragraph
            'ß' => Some(167), // German double s
            '®' => Some(168), // registered
            '©' => Some(169), // copyright
            '™' => Some(170), // trademark
            '´' => Some(171), // acute
            '¨' => Some(172), // diaeresis
            'Æ' => Some(174), // AE
            'Ø' => Some(175), // O slash
            '±' => Some(177), // plusminus
            '¥' => Some(180), // yen
            'µ' => Some(181), // micro
            'ª' => Some(187), // ordfeminine
            'º' => Some(188), // ordmasculine
            'æ' => Some(190), // ae
            'ø' => Some(191), // o slash
            '¿' => Some(192), // question down
            '¡' => Some(193), // exclamation down
            '¬' => Some(194), // not
            'ƒ' => Some(196), // florin
            '«' => Some(199), // left guille
            '»' => Some(200), // right guille
            '…' => Some(201), // ellipsis
            ' ' => Some(202), // non-breaking space
            'À' => Some(203), // A grave
            'Ã' => Some(204), // A tilde
            'Õ' => Some(205), // O tilde
            'Œ' => Some(206), // OE
            'œ' => Some(207), // oe
            '–' => Some(208), // endash
            '—' => Some(209), // emdash
            '“' => Some(210), // ldquo
            '”' => Some(211), // rdquo
            '‘' => Some(212), // lsquo
            '’' => Some(213), // rsquo
            '÷' => Some(214), // divide
            'ÿ' => Some(216), // y dieresis
            'Ÿ' => Some(217), // Y dieresis
            '⁄' => Some(218), // fraction
            '¤' => Some(219), // currency
            '‹' => Some(220), // left single guille
            '›' => Some(221), // right single guille
            'ﬁ' => Some(222), // fi
            'ﬂ' => Some(223), // fl
            '‡' => Some(224), // double dagger
            '·' => Some(225), // middle dot
            '‚' => Some(226), // single quote base
            '„' => Some(227), // double quote base
            '‰' => Some(228), // perthousand
            'Â' => Some(229), // A circumflex
            'Ê' => Some(230), // E circumflex
            'Á' => Some(231), // A acute
            'Ë' => Some(232), // E dieresis
            'È' => Some(233), // E grave
            'Í' => Some(234), // I acute
            'Î' => Some(235), // I circumflex
            'Ï' => Some(236), // I dieresis
            'Ì' => Some(237), // I grave
            'Ó' => Some(238), // O acute
            'Ô' => Some(239), // O circumflex
            'Ò' => Some(241), // O grave
            'Ú' => Some(242), // U acute
            'Û' => Some(243), // U circumflex
            'Ù' => Some(244), // U grave
            'ı' => Some(245), // dot-less i
            '^' => Some(246), // circumflex
            '˜' => Some(247), // tilde
            '¯' => Some(248), // macron
            '˘' => Some(249), // breve
            '˙' => Some(250), // dot accent
            '˚' => Some(251), // ring
            '¸' => Some(252), // cedilla
            '˝' => Some(253), // Hungarian umlaut (double acute accent)
            '˛' => Some(254), // ogonek
            'ˇ' => Some(255), // caron
            _ => None,
        }
    }
}
