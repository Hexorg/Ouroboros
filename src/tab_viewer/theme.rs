use egui::{Color32, RichText};

#[derive(Clone, Copy, PartialEq, enum_map::Enum)]
pub enum TokenType {
    Comment,
    Keyword,
    Symbol,
    Type,
    StringLiteral,
    NumericalLiteral,
    Punctuation,
    Whitespace,
}

#[derive(Clone, Copy)]
struct TokenStyle {
    color: Color32,
    strong: bool,
    underlined: bool,
}

#[derive(Clone)]
pub struct CodeTheme {
    dark_mode: bool,
    font_id: egui::FontId,
    formats: enum_map::EnumMap<TokenType, TokenStyle>,
    pub highlight_color: Color32,
}
impl CodeTheme {
    pub fn from_style(style: &egui::Style) -> Self {
        let font_id = style
            .override_font_id
            .clone()
            .unwrap_or_else(|| egui::TextStyle::Monospace.resolve(style));

        if style.visuals.dark_mode {
            Self::dark_with_font_id(font_id)
        } else {
            Self::light_with_font_id(font_id)
        }
    }
    fn dark_with_font_id(font_id: egui::FontId) -> Self {
        use egui::Color32;
        Self {
            font_id,
            dark_mode: true,
            formats: enum_map::enum_map![
                TokenType::Comment => TokenStyle{color:Color32::from_gray(120), strong:false, underlined:false},
                TokenType::Keyword => TokenStyle{color:Color32::from_rgb(255, 100, 100), strong:true, underlined:false},
                TokenType::Symbol => TokenStyle{color:Color32::from_rgb(200, 200, 171), strong:false, underlined:false},
                TokenType::Type => TokenStyle{color:Color32::from_rgb(109, 147, 226), strong:true, underlined:false},
                TokenType::StringLiteral => TokenStyle{color:Color32::from_rgb(109, 226, 147), strong:false, underlined:false},
                TokenType::NumericalLiteral => TokenStyle{color:Color32::from_rgb(16, 147, 226), strong:false, underlined:false},
                TokenType::Punctuation => TokenStyle{color:Color32::LIGHT_GRAY, strong:false, underlined:false},
                TokenType::Whitespace => TokenStyle{color:Color32::TRANSPARENT, strong:false, underlined:false},
            ],
            highlight_color: Color32::from_rgba_unmultiplied(255, 255, 224, 30),
        }
    }

    fn light_with_font_id(font_id: egui::FontId) -> Self {
        use egui::Color32;
        Self {
            font_id,
            dark_mode: false,
            formats: enum_map::enum_map![
                TokenType::Comment => TokenStyle{color:Color32::GRAY, strong:false, underlined:false},
                TokenType::Keyword => TokenStyle{color:Color32::from_rgb(235, 0, 0), strong:true, underlined:false},
                TokenType::Symbol => TokenStyle{color:Color32::from_rgb(153, 134, 255), strong:false, underlined:false},
                TokenType::Type => TokenStyle{color:Color32::from_rgb(109, 226, 147), strong:true, underlined:false},
                TokenType::StringLiteral => TokenStyle{color:Color32::from_rgb(37, 203, 105), strong:false, underlined:false},
                TokenType::NumericalLiteral => TokenStyle{color:Color32::from_rgb(16, 147, 226), strong:false, underlined:false},
                TokenType::Punctuation => TokenStyle{color:Color32::DARK_GRAY, strong:false, underlined:false},
                TokenType::Whitespace => TokenStyle{color:Color32::TRANSPARENT, strong:false, underlined:false},
            ],
            highlight_color: Color32::from_rgba_unmultiplied(40, 40, 10, 30),
        }
    }

    pub fn make_rich(&self, token: TokenType, string: impl Into<String>) -> RichText {
        let theme = &self.formats[token];
        let mut r = RichText::new(string)
            .font(self.font_id.clone())
            .color(theme.color);
        if theme.strong {
            r = r.strong()
        }
        if theme.underlined {
            r = r.underline()
        }
        r
    }
}
