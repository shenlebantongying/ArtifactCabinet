use std::io;
use std::path::PathBuf;

use crossterm::{event, execute, terminal};
use ratatui::{widgets};
use ratatui::prelude::*;

use clap::Parser;

type Term = Terminal<CrosstermBackend<io::Stdout>>;

fn tui_init() -> io::Result<Term> {
    execute!(io::stdout(),terminal::EnterAlternateScreen)?;
    terminal::enable_raw_mode()?;
    Terminal::new(CrosstermBackend::new(io::stdout()))
}

fn tui_restore() -> io::Result<()> {
    execute!(io::stdout(),terminal::LeaveAlternateScreen)?;
    terminal::disable_raw_mode()?;
    Ok(())
}

#[derive(clap::Parser)]
struct Cli {
    /// title of a link
    title: Option<String>,
    /// url of a link
    url: Option<String>,
}

struct Storage {
    conn: rusqlite::Connection,
}

impl Storage {
    fn new(db_path: PathBuf) -> Self {
        let t = rusqlite::Connection::open(db_path).expect("die");

        t.execute(
            include_str!("sql/create_db.sql"),
            (),
        ).expect("die");

        Storage {
            conn: t
        }
    }

    fn insert(&self, title: &str, url: &str) {
        match self.conn.execute(
            include_str!("sql/insert.sql"),
            (title, url),
        ) {
            Err(_) => panic!(),
            Ok(_) => ()
        };
    }

    fn get_rows(&self) -> Vec<Entry> {
        let mut stmt = self.conn.prepare(include_str!("sql/select_all_rows.sql")).expect("die");
        let rows = stmt.query_map([],
                                  |row| {
                                      Ok(Entry {
                                          title: row.get::<usize, String>(0).expect("die"),
                                          url: row.get::<usize, String>(1).expect("die"),
                                      })
                                  }).expect("die");

        let mut ret = vec![];

        for row in rows {
            match row {
                Ok(e) => { ret.push(e) }
                Err(_) => panic!("no")
            }
        }

        return ret;
    }
}


struct App<> {
    storage: Storage,
    exit: bool,
    table_state: widgets::TableState,
    rows: Vec<Entry>,
}

struct Entry {
    title: String,
    url: String,
}

impl App {
    fn new(s: Storage) -> Self {
        App {
            storage: s,
            exit: false,
            table_state: widgets::TableState::default().with_selected(0),
            rows: vec![],
        }
    }

    fn run(&mut self, terminal: &mut Term) -> io::Result<()> {
        self.update_rows();

        while !self.exit {
            terminal.draw(
                |frame| self.render_frame(frame))?;
            self.handle_events()?;
        }
        Ok(())
    }

    fn render_frame(&mut self, frame: &mut Frame) {
        let widths = [Constraint::Percentage(20), Constraint::Percentage(60), Constraint::Percentage(20)];

        let table = widgets::Table::new(
            self.rows.iter().map(|r| widgets::Row::new(vec![r.title.as_str(), r.url.as_str()])),
            widths)
            .column_spacing(2)
            .style(Style::new().blue())
            .block(widgets::Block::default().title("Table"))
            .highlight_style(Style::new().red().italic())
            .highlight_symbol(">>");

        frame.render_stateful_widget(table, frame.size(), &mut self.table_state);
    }

    fn handle_events(&mut self) -> io::Result<()> {
        match event::read()? {
            event::Event::Key(key_event) if key_event.kind == event::KeyEventKind::Press => {
                match key_event.code {
                    event::KeyCode::Char('q') => self.exit(),
                    event::KeyCode::Char('s') => self.move_down(),
                    event::KeyCode::Char('w') => self.move_up(),
                    event::KeyCode::Char('d') => self.open(),
                    _ => {}
                }
            }
            _ => {}
        };
        Ok(())
    }

    fn open(&self) {
        let mut cmd = if cfg!(target_os = "linux") {
            std::process::Command::new("xdg-open")
        } else if cfg!(target_os = "macos") {
            std::process::Command::new("open")
        } else {
            panic!()
        };

        match self.table_state.selected() {
            Some(n) => {
                cmd.arg(&self.rows[n].url).spawn().expect("die");
            }
            None => panic!()
        }
    }

    fn move_down(&mut self) {
        match self.table_state.selected() {
            Some(n) => {
                if n < self.rows.len() - 1 {
                    self.table_state.select(Option::from(n + 1));
                }
            }
            None => {
                self.table_state.select(Option::from(0));
            }
        }
    }

    fn update_rows(&mut self) {
        self.rows = self.storage.get_rows();
    }

    fn move_up(&mut self) {
        match self.table_state.selected() {
            Some(n) => {
                if n > 0 {
                    self.table_state.select(Option::from(n - 1));
                } else {
                    self.table_state.select(Option::from(0));
                }
            }
            None => {
                self.table_state.select(Option::from(0));
            }
        }
    }

    fn exit(&mut self) {
        self.exit = true;
    }
}


fn main() -> io::Result<()>
{
    let s = Storage::new(
        match std::env::current_dir() {
            Ok(mut t) => {
                t.push("test.db");
                t
            }
            Err(_) => panic!("no db"),
        }
    );

    if std::env::args_os().count() == 3 {
        let cli = Cli::parse();
        match (cli.title.as_deref(), cli.url.as_deref()) {
            (Some(t), Some(u)) => s.insert(t, u),
            _ => panic!()
        }
    }


    let mut terminal = tui_init()?;

    let app_result = App::new(s).run(&mut terminal);

    tui_restore()?;

    app_result
}
