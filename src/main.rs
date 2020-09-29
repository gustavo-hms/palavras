use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::{collections::HashMap, fs::File};

struct Word {
    root: String,
    flags: Option<String>,
}

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
    let aff = File::open("pt_BR.aff")?;
    let affixes = build_affixes_table(BufReader::new(aff));

    let dic = File::open("pt_BR.dic")?;
    let words = BufReader::new(dic)
        .lines()
        .skip(1)
        .filter_map(|line| line.ok())
        .map(|mut root| {
            let flags = root.find("/").map(|i| root.split_off(i));
            Word { root, flags }
        });

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

        let (affix_type, name, cross_product, count) =
            match (tokens.next(), tokens.next(), tokens.next(), tokens.next()) {
                (Some(a), Some(b), Some(c), Some(d)) => (a, b, c, d.parse::<usize>().unwrap()),

                _ => panic!("Arquivo corrompido!"),
            };

        let mut affixes = Vec::with_capacity(count);

        for _ in 0..count {
            let affix = parse_affix(affix_type, name, cross_product, self.iter.next()?);
            affixes.push(affix);
        }

        match affix_type {
            "PFX" => Some((name.to_string(), AffixClass::Prefix(affixes))),
            _ => Some((name.to_string(), AffixClass::Suffix(affixes))),
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
            (c.to_string(), d.to_string(), e.to_string())
        }

        _ => panic!("Arquivo corrompido!"),
    };

    let cross_product = match cross_product {
        "Y" => true,
        "N" => false,
        _ => panic!("Arquivo corrompido!"),
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
