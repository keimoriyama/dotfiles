#import "@preview/touying:0.6.1": *
#import themes.university: *
// #import "university.typ": *
#set text(font: "Times New Roman") // 全体フォント指定（明朝体＝セリフ体）
#show regex("[\p{scx:Han}\p{scx:Hira}\p{scx:Kana}]"): set text(
  font: "YuKyokasho Yoko",
) // 漢字かなカナのみ指定（ゴシック体＝サン?リフ体）

#show: university-theme.with(
  aspect-ratio: "16-9",
  // align: horizon,
  // config-common(handout: true),
  config-info(
    title: [Title],
    subtitle: [Subtitle],
    author: [Authors],
    date: datetime.today(),
    institution: [Institution],
    // logo: emoji.school,
  ),
  config-colors(
    primary: rgb("#04364A"),
    secondary: rgb("#04364A"),
    tertiary: rgb("#04366A"),
  ),
  progress-bar: false,
  footer-a: self => none,
  footer-b: self => none,
  footer-c: self => {
    h(1fr)
    utils.display-info-date(self)
    h(1fr)
    context utils.slide-counter.display() + " / " + utils.last-slide-number
    h(1fr)
  },
)

#title-slide()

{{__cursor__}}
