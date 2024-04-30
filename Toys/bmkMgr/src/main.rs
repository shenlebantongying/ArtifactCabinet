use std::io;
use std::io::{Stdout, stdout};
use std::path::PathBuf;

use crossterm::event::{self, Event, KeyCode, KeyEventKind};
use crossterm::execute;
use crossterm::terminal::{disable_raw_mode,
                          enable_raw_mode,
                          EnterAlternateScreen,
                          LeaveAlternateScreen};
use ratatui::{prelude::*, widgets};
use ratatui::layout::Constraint::Percentage;

use rusqlite::*;

type Term = Terminal<CrosstermBackend<Stdout>>;

fn tui_init() -> io::Result<Term> {
    execute!(stdout(),EnterAlternateScreen)?;
    enable_raw_mode()?;
    Terminal::new(CrosstermBackend::new(stdout()))
}

fn tui_restore() -> io::Result<()> {
    execute!(stdout(),LeaveAlternateScreen)?;
    disable_raw_mode()?;
    Ok(())
}

struct App<'a> {
    db_conn: Connection,
    exit: bool,
    table_state: widgets::TableState,
    rows: Vec<widgets::Row<'a>>,
}

struct Entry {
    id: u64,
    name: String,
}

impl App<'_> {
    fn new(db_path: &PathBuf) -> Self {
        let con = Connection::open(db_path).expect("die");

        con.execute(
            include_str!("sql/create_db.sql")
            ,
            (),
        ).expect("die");

        App {
            db_conn: con,
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
        let widths = [Percentage(20), Percentage(60), Percentage(20)];
        let table = widgets::Table::new(self.rows.clone(), widths) // TODO:
            .column_spacing(2)
            .style(Style::new().blue())
            .block(widgets::Block::default().title("Table"))
            .highlight_style(Style::new().red().italic())
            .highlight_symbol(">>");

        frame.render_stateful_widget(table, frame.size(), &mut self.table_state);
    }

    fn handle_events(&mut self) -> io::Result<()> {
        match event::read()? {
            Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                match key_event.code {
                    KeyCode::Char('q') => self.exit(),
                    KeyCode::Char('s') => self.move_down(),
                    KeyCode::Char('w') => self.move_up(),
                    _ => {}
                }
            }
            _ => {}
        };
        Ok(())
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
        let mut stmt = self.db_conn.prepare(include_str!("sql/select_all_rows.sql")).expect("die");
        let rows = stmt.query_map([],
                                  |row| {
                                      Ok(Entry {
                                          id: row.get::<usize, u64>(0).expect("die"),
                                          name: row.get::<usize, String>(1).expect("die"),
                                      })
                                  }).expect("die");

        self.rows.clear();

        for row in rows {
            match row {
                Ok(e) => { self.rows.push(widgets::Row::new(vec![e.id.to_string(), e.name])) }
                Err(_) => panic!("no")
            }
        }
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
    let db_path = match std::env::current_dir() {
        Ok(mut t) => {
            t.push("test.db");
            t
        }
        Err(_) => panic!("no db"),
    };

    let mut terminal = tui_init()?;

    let app_result = App::new(&db_path).run(&mut terminal);

    tui_restore()?;

    app_result
}