use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::{collections::HashMap, fs::File};

struct Word {
    radix: String,
    flags: Vec<char>,
}

#[derive(Default)]
struct Affix {
    stripping: String,
    affix: String,
    condition: String,
    cross_product: bool,
}

enum AffixClass {
    Prefix(Vec<Affix>),
    Suffix(Vec<Affix>),
}

fn main() -> io::Result<()> {
    let file = File::open("pt_BR.aff")?;
    let affixes = build_affixes_table(BufReader::new(file));

    Ok(())
}

fn build_affixes_table(buffer: BufReader<File>) -> HashMap<String, AffixClass> {
    buffer
        .lines()
        .filter_map(|line| line.ok())
        .filter(|line| line.starts_with("SFX") || line.starts_with("PFX"))
        .parse_affix_class()
        .fold(HashMap::new(), |mut affixes, (name, affix_class)| {
            affixes.insert(name, affix_class);
            affixes
        })
}

struct AffixIterator<I> {
    iter: I,
}

impl<I: Iterator<Item = String>> Iterator for AffixIterator<I> {
    type Item = (String, AffixClass);

    fn next(&mut self) -> Option<Self::Item> {
        let line = self.iter.next()?;
        let mut tokens = line.split_whitespace();
        let (affix_type, name, cross_product, count): (&str, &str, &str, usize) =
            match (tokens.next(), tokens.next(), tokens.next(), tokens.next()) {
                (Some(a), Some(b), Some(c), Some(d)) => (a, b, c, String::from(d).parse().unwrap()),

                _ => panic!("Arquivo corrompido!"),
            };

        let mut affixes = Vec::with_capacity(count);
        for _ in 0..count {
            let affix = parse_affix(affix_type, name, cross_product, self.iter.next()?);
            affixes.push(affix);
        }

        match affix_type {
            "PFX" => Some((String::from(name), AffixClass::Prefix(affixes))),
            _ => Some((String::from(name), AffixClass::Suffix(affixes))),
        }
    }
}

fn parse_affix(typ: &str, name: &str, cross_product: &str, line: String) -> Affix {
    let mut tokens = line.split_whitespace();
    let (stripping, affix, condition) = match (
        tokens.next(),
        tokens.next(),
        tokens.next(),
        tokens.next(),
        tokens.next(),
    ) {
        (Some(a), Some(b), Some(c), Some(d), Some(e)) if a == typ && b == name => {
            (String::from(c), String::from(d), String::from(e))
        }

        _ => panic!("Arquivo corrompido!"),
    };

    let cross_product = match cross_product {
        "Y" => true,
        _ => false,
    };

    Affix {
        stripping,
        affix,
        condition,
        cross_product,
    }
}

trait AffixIteratorAdapter: Iterator<Item = String> {
    fn parse_affix_class(self) -> AffixIterator<Self>
    where
        Self: Sized,
    {
        AffixIterator { iter: self }
    }
}

impl<I> AffixIteratorAdapter for I where I: Iterator<Item = String> {}
