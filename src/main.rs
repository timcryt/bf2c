use std::{cmp, collections::BTreeMap, fs::File, iter::Peekable};
use std::{collections::BTreeSet, io::prelude::*};

use cmp::{Ord, Ordering};
use Ordering::{Equal, Greater, Less};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SimpleInst {
    Next,
    Prev,
    Incr,
    Decr,
    Prnt,
    Inpt,
    Dolp,
    Relp,
}

impl SimpleInst {
    fn new(c: char) -> Option<SimpleInst> {
        use SimpleInst::*;
        match c {
            '>' => Some(Next),
            '<' => Some(Prev),
            '+' => Some(Incr),
            '-' => Some(Decr),
            '.' => Some(Prnt),
            ',' => Some(Inpt),
            '[' => Some(Dolp),
            ']' => Some(Relp),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct Code(Vec<SimpleInst>);

impl Code {
    fn new(s: String) -> Result<Code, SimpleInst> {
        let mut d = 0;
        let mut insts = Vec::new();
        for inst in s.chars().filter_map(|c| SimpleInst::new(c)) {
            match inst {
                SimpleInst::Dolp => d += 1,
                SimpleInst::Relp => {
                    d -= 1;
                    if d < 0 {
                        return Err(SimpleInst::Relp);
                    }
                }
                _ => {}
            }
            insts.push(inst);
        }
        if d > 0 {
            Err(SimpleInst::Dolp)
        } else {
            let mut c = Code(insts);
            c.emit_unreachable();
            Ok(c)
        }
    }

    fn emit_unreachable(&mut self) {
        //use SimpleInst::*;
        let mut emit = false;
        let mut null = true;
        /*eprintln!(
            "{:?}",
            self.0
                .iter()
                .map(|x| match x {
                    Next => ">",
                    Prev => "<",
                    Incr => "+",
                    Decr => "-",
                    Prnt => ".",
                    Inpt => ",",
                    Dolp => "[",
                    Relp => "]",
                })
                .collect::<String>()
        );*/
        self.0.retain(|inst| {
            if matches!(inst, SimpleInst::Dolp) && null {
                emit = true;
                false
            } else if matches!(inst, SimpleInst::Relp) && emit {
                emit = false;
                null = true;
                false
            } else if matches!(inst, SimpleInst::Relp) {
                null = true;
                true
            } else if !matches!(inst, SimpleInst::Relp | SimpleInst::Dolp | SimpleInst::Prnt) {
                null = false;
                !emit
            } else {
                !emit
            }
        });
        /*eprintln!(
            "{:?}",
            self.0
                .iter()
                .map(|x| match x {
                    Next => ">",
                    Prev => "<",
                    Incr => "+",
                    Decr => "-",
                    Prnt => ".",
                    Inpt => ",",
                    Dolp => "[",
                    Relp => "]",
                })
                .collect::<String>()
        );*/
    }
}

impl std::fmt::Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use SimpleInst::*;
        match self
            .0
            .iter()
            .filter_map(|x| {
                write!(
                    f,
                    "{}",
                    match x {
                        Next => '>',
                        Prev => '<',
                        Incr => '+',
                        Decr => '-',
                        Prnt => '.',
                        Inpt => ',',
                        Dolp => '[',
                        Relp => ']',
                    }
                )
                .err()
            })
            .next()
        {
            Some(x) => Err(x),
            None => Ok(()),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum ExtendInst {
    Mov(isize),
    Add(isize),
    Prt,
    Ipt,
    Dlp,
    Rlp,
}

impl ExtendInst {
    fn new(i: &mut Peekable<impl Iterator<Item = SimpleInst>>) -> Option<ExtendInst> {
        match i.next() {
            None => None,
            Some(inst)
                if inst == SimpleInst::Prnt
                    || inst == SimpleInst::Inpt
                    || inst == SimpleInst::Dolp
                    || inst == SimpleInst::Relp =>
            {
                Some(match inst {
                    SimpleInst::Prnt => ExtendInst::Prt,
                    SimpleInst::Inpt => ExtendInst::Ipt,
                    SimpleInst::Dolp => ExtendInst::Dlp,
                    SimpleInst::Relp => ExtendInst::Rlp,
                    _ => unreachable!(),
                })
            }

            Some(inst) if inst == SimpleInst::Incr || inst == SimpleInst::Decr => {
                let mut d = if inst == SimpleInst::Incr { 1 } else { -1 };
                loop {
                    match i.peek() {
                        Some(SimpleInst::Incr) => d += 1,
                        Some(SimpleInst::Decr) => d -= 1,
                        _ => break,
                    }
                    i.next();
                }
                Some(ExtendInst::Add(d))
            }

            Some(inst) if inst == SimpleInst::Next || inst == SimpleInst::Prev => {
                let mut d = if inst == SimpleInst::Next { 1 } else { -1 };
                loop {
                    match i.peek() {
                        Some(SimpleInst::Next) => d += 1,
                        Some(SimpleInst::Prev) => d -= 1,
                        _ => break,
                    }
                    i.next();
                }
                Some(ExtendInst::Mov(d))
            }

            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
struct ExtendCode(Vec<ExtendInst>);

impl ExtendCode {
    fn new(c: Code) -> ExtendCode {
        let mut i = c.0.into_iter().peekable();
        ExtendCode(std::iter::from_fn(|| ExtendInst::new(&mut i)).collect())
    }
}

#[derive(Clone, Debug)]
enum LinInst {
    Mov(isize),
    Add(isize),
    Prt,
    Ipt,
    Dlp,
    Rlp,
    Lin(BTreeMap<isize, isize>),
}

#[derive(Debug)]
struct LinCode(Vec<LinInst>);

impl LinCode {
    fn new(c: ExtendCode) -> LinCode {
        let mut i = 0;
        let mut r = Vec::new();
        while i < c.0.len() {
            match c.0[i] {
                ExtendInst::Dlp => match Self::loop_check(&c.0[i + 1..]) {
                    Some((m, d)) => {
                        r.push(LinInst::Lin(m));
                        i += d + 1;
                    }

                    None => {
                        r.push(LinInst::Dlp);
                    }
                },
                ExtendInst::Rlp => r.push(LinInst::Rlp),

                ExtendInst::Mov(n) => r.push(LinInst::Mov(n)),
                ExtendInst::Add(n) => r.push(LinInst::Add(n)),
                ExtendInst::Prt => r.push(LinInst::Prt),
                ExtendInst::Ipt => r.push(LinInst::Ipt),
            }
            i += 1;
        }
        LinCode(r)
    }

    fn loop_check(c: &[ExtendInst]) -> Option<(BTreeMap<isize, isize>, usize)> {
        let mut m = BTreeMap::new();
        let mut d = 0;
        m.insert(0, 0);

        for i in c.into_iter().enumerate() {
            match *i.1 {
                ExtendInst::Dlp => return None,
                ExtendInst::Rlp => {
                    return if m[&0] == -1 {
                        m.remove(&0);
                        Some((m, i.0))
                    } else {
                        None
                    }
                }
                ExtendInst::Add(n) => {
                    m.insert(d, m.get(&d).unwrap_or(&0) + n);
                }
                ExtendInst::Mov(n) => d += n,
                _ => return None,
            }
        }
        unreachable!();
    }
}

#[derive(Debug, Clone)]
enum OptLinInst {
    OptLin {
        data: BTreeMap<isize, (BTreeMap<isize, isize>, isize)>,
        mov: isize,
    },
    Prt,
    Ipt,
    Dlp,
    Rlp,
}

#[derive(Debug)]
enum NullInf {
    Unknown,
    This,
    All,
}

impl OptLinInst {
    fn new_lin() -> OptLinInst {
        let mut t = BTreeMap::new();
        t.insert(0, {
            let mut t = BTreeMap::new();
            t.insert(0, 1);
            (t, 0)
        });
        OptLinInst::OptLin { data: t, mov: 0 }
    }

    fn add_lin(&mut self, (d, li): (isize, (BTreeMap<isize, isize>, isize))) {
        let optlin = match self {
            OptLinInst::OptLin { data: d, .. } => d,
            _ => unreachable!(),
        };
        let mut nli = BTreeMap::new();
        let mut nd = li.1;
        for (i, di) in li.0 {
            let mut f = true;
            if optlin.get(&i).is_some() {
                for (j, dj) in &optlin[&i].0 {
                    *nli.entry(*j).or_insert(0) += dj * di;
                    f = false;
                }
                nd += optlin[&i].1 * di;
            }
            if f {
                *nli.entry(i).or_insert(0) += di;
            }
        }

        optlin.entry(d).or_insert({
            let mut t = BTreeMap::new();
            t.insert(d, 1);
            (t, 0)
        });
        for (i, di) in nli {
            *optlin.get_mut(&d).unwrap().0.entry(i).or_insert(0) += di;
        }
        optlin.get_mut(&d).unwrap().1 += nd;
    }

    fn nul_lin(&mut self, d: isize) {
        let optlin = match self {
            OptLinInst::OptLin { data: d, .. } => d,
            _ => unreachable!(),
        };
        optlin
            .entry(d)
            .and_modify(|x| *x = (BTreeMap::new(), 0))
            .or_insert((BTreeMap::new(), 0));
    }

    fn set_mov(&mut self, d: isize) {
        match self {
            OptLinInst::OptLin { mov, .. } => *mov += d,
            _ => unreachable!(),
        };
    }

    fn topsort(&self) -> Option<Vec<isize>> {
        let mut v = Vec::new();
        let mut grey = BTreeSet::new();
        let mut black = BTreeSet::new();
        fn dfs(
            g: &BTreeMap<isize, (BTreeMap<isize, isize>, isize)>,
            i: isize,
            v: &mut Vec<isize>,
            grey: &mut BTreeSet<isize>,
            black: &mut BTreeSet<isize>,
        ) -> bool {
            grey.insert(i);
            for u in g[&i].0.keys().filter(|u| **u != i) {
                if g.contains_key(u) && !black.contains(u) && !grey.contains(u) {
                    if !dfs(g, *u, v, grey, black) {
                        return false;
                    }
                } else if grey.contains(u) {
                    return false;
                }
            }
            grey.remove(&i);
            black.insert(i);
            v.push(i);
            true
        }

        let g = match self {
            OptLinInst::OptLin { data: d, .. } => d,
            _ => unreachable!(),
        };

        for i in g.keys() {
            if !black.contains(i) {
                if !dfs(g, *i, &mut v, &mut grey, &mut black) {
                    return None;
                }
            }
        }
        Some(v)
    }

    fn zip(&mut self, inf: NullInf) {
        match self {
            OptLinInst::OptLin { data, .. } => {
                let mut trashbox_i = Vec::new();
                for (i, di) in data.iter_mut() {
                    let mut trashbox_j = Vec::new();

                    if matches!(inf, NullInf::All) && di.1 == 0 {
                        trashbox_i.push(*i);
                    }

                    for (j, dj) in &di.0 {
                        if *dj == 0
                            || matches!(inf, NullInf::All)
                            || (matches!(inf, NullInf::This) && *j == 0 && *i != 0)
                        {
                            trashbox_j.push(*j);
                        }
                    }

                    for j in trashbox_j {
                        di.0.remove(&j);
                    }

                    if di.0.len() == 1 && matches!(di.0.get(i), Some(1)) && di.1 == 0 {
                        trashbox_i.push(*i);
                    }
                }
                for i in trashbox_i {
                    data.remove(&i);
                }
            }

            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
struct OptLinCode(Vec<OptLinInst>);

impl OptLinCode {
    fn new(l: LinCode) -> OptLinCode {
        let mut opt_r = 0..0;
        let mut v = OptLinCode(Vec::new());
        let mut ntf = 0;
        for li in &l.0 {
            match li {
                LinInst::Mov(_) | LinInst::Add(_) | LinInst::Lin(_) => {
                    opt_r = opt_r.start..opt_r.end + 1;
                }

                li @ LinInst::Prt | li @ LinInst::Ipt | li @ LinInst::Dlp | li @ LinInst::Rlp => {
                    while opt_r.start < opt_r.end {
                        let (inst, rend) = Self::opt_lin(
                            &l.0[opt_r.clone()],
                            if opt_r.start == 0 {
                                NullInf::All
                            } else if ntf > 0 {
                                NullInf::This
                            } else {
                                NullInf::Unknown
                            },
                        );
                        v.0.push(inst);
                        opt_r = (opt_r.start + rend)..(opt_r.end);
                    }
                    opt_r = (opt_r.end + 1)..(opt_r.end + 1);
                    v.0.push(match li {
                        LinInst::Prt => OptLinInst::Prt,
                        LinInst::Ipt => OptLinInst::Ipt,
                        LinInst::Dlp => OptLinInst::Dlp,
                        LinInst::Rlp => {
                            ntf = 2;
                            OptLinInst::Rlp
                        }
                        _ => unreachable!(),
                    });

                    ntf = 0.max(ntf - 1);
                }
            }
        }
        while opt_r.start != opt_r.end {
            let (inst, rend) = Self::opt_lin(
                &l.0[opt_r.clone()],
                if opt_r.start == 0 {
                    NullInf::All
                } else if ntf > 0 {
                    NullInf::This
                } else {
                    NullInf::Unknown
                },
            );
            v.0.push(inst);
            opt_r = (opt_r.start + rend)..(opt_r.end);
        }
        v
    }

    fn opt_lin(lr: &[LinInst], inf: NullInf) -> (OptLinInst, usize) {
        let mut old = OptLinInst::new_lin();
        let mut oold = OptLinInst::new_lin();
        let mut delta = 0;
        for (i, li) in lr.into_iter().enumerate() {
            match li {
                LinInst::Mov(d) => delta += d,
                LinInst::Add(d) => {
                    old.add_lin((delta, (BTreeMap::new(), *d)));
                }
                LinInst::Lin(m) => {
                    for d in m {
                        old.add_lin((delta + d.0, {
                            let mut t = BTreeMap::new();
                            t.insert(delta, *d.1);
                            (t, 0)
                        }))
                    }
                    old.nul_lin(delta);
                }
                _ => {
                    unreachable!()
                }
            }
            if old.topsort().is_none() {
                oold.set_mov(delta);
                oold.zip(inf);
                return (oold, i);
            }
            oold = old.clone();
        }

        old.set_mov(delta);
        old.zip(inf);
        (old, lr.len())
    }
}

trait BfCode {
    fn write_as_c(&self, stream: &mut impl std::io::Write) -> Result<(), std::io::Error>;
}

impl BfCode for LinCode {
    fn write_as_c(&self, stream: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        writeln!(stream, "#include <stdio.h>")?;
        writeln!(stream, "#include <stdlib.h>")?;
        writeln!(stream, "unsigned char a[65536]; size_t i;")?;
        write!(stream, "int main(){{")?;
        for i in &self.0 {
            match i {
                LinInst::Mov(n) => {
                    if *n > 0 {
                        write!(stream, "i+={};", n)?
                    } else {
                        write!(stream, "i-={};", -n)?
                    }
                }
                LinInst::Add(n) => {
                    if *n > 0 {
                        write!(stream, "a[i]+={};", n)?
                    } else {
                        write!(stream, "a[i]-={};", -n)?
                    }
                }
                LinInst::Prt => write!(stream, "putchar(a[i]);")?,
                LinInst::Ipt => write!(stream, "a[i]=getchar();")?,
                LinInst::Dlp => write!(stream, "while(a[i]){{")?,
                LinInst::Rlp => write!(stream, "}}")?,
                LinInst::Lin(m) => {
                    for (o, d) in m {
                        write!(
                            stream,
                            "a[i{:+}]{asign}=a[i]{mulop};",
                            o,
                            asign = (if *d < 0 { "-" } else { "+" }),
                            mulop = (if *d == 1 || *d == -1 {
                                "".to_string()
                            } else {
                                format!("*{}", d.abs())
                            })
                        )?;
                    }
                    write!(stream, "a[i]=0;")?;
                }
            }
        }
        writeln!(stream, "}}")?;
        Ok(())
    }
}

impl BfCode for OptLinCode {
    fn write_as_c(&self, stream: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        writeln!(stream, "#include <stdio.h>")?;
        writeln!(stream, "#include <stdlib.h>")?;
        writeln!(stream, "unsigned char a[65536]; size_t i;")?;
        write!(stream, "int main(){{")?;
        for inst in &self.0 {
            match inst {
                OptLinInst::OptLin { .. } => {
                    let v = inst.topsort().unwrap();
                    let (data, mov) = match inst {
                        OptLinInst::OptLin { data, mov } => (data, mov),
                        _ => unreachable!(),
                    };
                    for i in v.into_iter().rev() {
                        match Ord::cmp(&i, &0) {
                            Less => write!(stream, "a[i-{}]", -i)?,
                            Equal => write!(stream, "a[i]")?,
                            Greater => write!(stream, "a[i+{}]", i)?,
                        }
                        let g = if let Some(1) = data[&i].0.get(&i) {
                            if data[&i].0.len() != 1 {
                                write!(stream, "+=")?;
                            }
                            false
                        } else {
                            write!(stream, "=")?;
                            true
                        };
                        let mut f = false;
                        for j in data[&i].0.iter().filter(|(&j, _)| g || (j != i)) {
                            if f {
                                write!(stream, "+")?;
                            }
                            f = true;
                            match Ord::cmp(j.0, &0) {
                                Less => write!(stream, "a[i-{}]", -j.0)?,
                                Equal => write!(stream, "a[i]")?,
                                Greater => write!(stream, "a[i+{}]", j.0)?,
                            }
                            if *j.1 != 1 {
                                write!(stream, "*{}", j.1)?;
                            }
                        }
                        let d = if !g && !f {
                            if data[&i].1 >= 0 {
                                write!(stream, "+=")?;
                                data[&i].1
                            } else {
                                write!(stream, "-=")?;
                                -data[&i].1
                            }
                        } else {
                            data[&i].1
                        };
                        match Ord::cmp(&d, &0) {
                            Less => write!(stream, "-{};", -d)?,
                            Equal => write!(stream, "{}", if f { ";" } else { "0;" })?,
                            Greater => write!(stream, "{}{};", if f { "+" } else { "" }, d)?,
                        }
                    }
                    match Ord::cmp(mov, &0) {
                        Less => write!(stream, "i-={};", -mov)?,
                        Equal => (),
                        Greater => write!(stream, "i+={};", mov)?,
                    }
                }

                OptLinInst::Prt => {
                    write!(stream, "putchar(a[i]);")?;
                }

                OptLinInst::Ipt => {
                    write!(stream, "a[i]=getchar();")?;
                }

                OptLinInst::Dlp => {
                    write!(stream, "while(a[i]){{")?;
                }

                OptLinInst::Rlp => {
                    write!(stream, "}}")?;
                }
            }
        }
        writeln!(stream, "}}")?;
        Ok(())
    }
}

fn main() {
    use clap::{App, Arg};

    let matches = App::new("BF code translator")
        .version("0.1.0")
        .author("timcryt <tymcrit@gmail.com>")
        .about("Translates brainfuck code to C with some optimizations")
        .arg(Arg::with_name("input")
            .short("i")
            .value_name("FILE")
            .help("input file")
            .takes_value(true)
            .required(true)
        )
        .arg(Arg::with_name("output")
            .short("o")
            .value_name("FILE")
            .help("output file")
            .takes_value(true)
            .required(true)
        )
        .arg(Arg::with_name("compress")
            .long("compress")
            .help("compress brainfuck code only, don't translate")
            .conflicts_with("optlevel")
        )
        .arg(Arg::with_name("optlevel")
            .short("O")
            .help("set optimization level\n 2 - linear optimizations\n 3 - linear sequences optimizations")
            .takes_value(true)
            .validator(|x| x.parse::<u8>().ok().filter(|x| (2..=3).contains(x)).map(|_| ()).ok_or(format!("Expected '2' or '3', found {}", x)))
            .conflicts_with("compress")
        )
        .get_matches()
    ;
    let inpfile = matches.value_of("input").unwrap();
    let outfile = matches.value_of("output").unwrap();

    let mut code = String::new();
    File::open(inpfile)
        .unwrap()
        .read_to_string(&mut code)
        .unwrap();

    let code = Code::new(code).unwrap();

    let mut outfile = File::create(outfile).unwrap();

    if matches.is_present("compress") {
        write!(outfile, "{}", code).unwrap();
    } else {
        let optlevel = matches.value_of("optlevel").map(|x| x.parse::<u8>().unwrap()).unwrap_or(2);
        match optlevel {
            2 => {
                let optcode = LinCode::new(ExtendCode::new(code));
                optcode.write_as_c(&mut outfile).unwrap();
            }

            3 => {
                let optcode = OptLinCode::new(LinCode::new(ExtendCode::new(code)));
                optcode.write_as_c(&mut outfile).unwrap();
            }

            _ => unreachable!(),
        }
    }
}
