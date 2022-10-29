katakana <- function(unicode.convert = FALSE, include.voiced = TRUE, include.psound = TRUE) {
  # katakana set
  katakana.all <- c(
    "ア", "イ", "ウ", "エ", "オ",
    "カ", "キ", "ク", "ケ", "コ",
    "サ", "シ", "ス", "セ", "ソ",
    "タ", "チ", "ツ", "テ", "ト",
    "ナ", "ニ", "ヌ", "ネ", "ノ",
    "ハ", "ヒ", "フ", "ヘ", "ホ",
    "マ", "ミ", "ム", "メ", "モ",
    "ヤ", "ユ", "ヨ",
    "ラ", "リ", "ル", "レ", "ロ",
    "ワ", "ヲ", "ン"
  )
  if (include.voiced) {
    katakana.all <- c(
      katakana.all,
      c(
        "ヴ",
        "ガ", "ギ", "グ", "ゲ", "ゴ",
        "ザ", "ジ", "ズ", "ゼ", "ゾ",
        "ダ", "ヂ", "ヅ", "デ", "ド",
        "バ", "ビ", "ブ", "ベ", "ボ"
      )
    )
  }
  if (include.psound) {
    katakana.all <- c(
      katakana.all,
      c(
        "パ", "ピ", "プ", "ぺ", "ポ"
      )
    )
  }
  if (unicode.convert) {
    katakana.all <- stringi::stri_escape_unicode(katakana.all)
  }
  return(katakana.all)
}