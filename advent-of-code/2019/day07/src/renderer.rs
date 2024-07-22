use std::str::FromStr;

use ratatui::{
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    text::{Span, Text},
    widgets::{
        block::Title, Block, BorderType, Borders, Cell, List, Paragraph, Row, Scrollbar,
        ScrollbarOrientation, ScrollbarState, Table, TableState, Tabs,
    },
    Frame,
};

use crate::{app::App, instruction::Instruction, process};

#[allow(dead_code)]
enum Monokai {
    DarkBlack,
    LightBlack,
    Background,
    DarkerGrey,
    DarkGrey,
    Grey,
    LightGrey,
    LighterGrey,
    White,
    Blue,
    Green,
    Violet,
    Orange,
    Red,
    Yellow,
}

impl From<Monokai> for Color {
    fn from(color: Monokai) -> Self {
        match color {
            Monokai::DarkBlack => Color::from_str("#19181a").unwrap(),
            Monokai::LightBlack => Color::from_str("#221f22").unwrap(),
            Monokai::Background => Color::from_str("#2d2a2e").unwrap(),
            Monokai::DarkerGrey => Color::from_str("#403e41").unwrap(),
            Monokai::DarkGrey => Color::from_str("#5b595c").unwrap(),
            Monokai::Grey => Color::from_str("#727072").unwrap(),
            Monokai::LightGrey => Color::from_str("#939293").unwrap(),
            Monokai::LighterGrey => Color::from_str("#c1c0c0").unwrap(),
            Monokai::White => Color::from_str("#fcfcfa").unwrap(),
            Monokai::Blue => Color::from_str("#78dce8").unwrap(),
            Monokai::Green => Color::from_str("#a9dc76").unwrap(),
            Monokai::Violet => Color::from_str("#ab9df2").unwrap(),
            Monokai::Orange => Color::from_str("#fc9867").unwrap(),
            Monokai::Red => Color::from_str("#ff6188").unwrap(),
            Monokai::Yellow => Color::from_str("#ffd866").unwrap(),
        }
    }
}

pub struct RendererState {
    pub active_process: usize,
    pub total_processes: usize,
    memory_rows: Vec<usize>,
    table_states: Vec<TableState>,
    scroll_states: Vec<ScrollbarState>,
}

impl RendererState {
    pub fn new(app: &App) -> Self {
        let states = app.states();
        let total_processes = states.len();
        let memory_rows = states
            .iter()
            .map(|state| state.memory.len() / 8)
            .collect::<Vec<_>>();
        let scroll_states = memory_rows
            .iter()
            .map(|i| ScrollbarState::new(*i))
            .collect::<Vec<_>>();
        let table_states = vec![TableState::default(); total_processes];
        Self {
            active_process: 0,
            total_processes,
            memory_rows,
            table_states,
            scroll_states,
        }
    }

    pub fn scroll_up(&mut self) {
        self.scroll_states[self.active_process].prev();
        let table_state = &mut self.table_states[self.active_process];
        if table_state.offset() > 0 {
            table_state.select(Some(table_state.offset() - 1));
            *table_state.offset_mut() -= 1;
        }
    }

    pub fn scroll_down(&mut self) {
        self.scroll_states[self.active_process].next();
        let table_state = &mut self.table_states[self.active_process];
        if table_state.offset() < self.memory_rows[self.active_process] / 8 {
            table_state.select(Some(table_state.offset() + 1));
            *table_state.offset_mut() += 1;
        }
    }

    pub fn render(&mut self, app: &App, frame: &mut Frame<'_>) {
        let rows = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Length(1),
                    Constraint::Length(3),
                    Constraint::Min(1),
                    Constraint::Length(1),
                ]
                .as_ref(),
            )
            .split(frame.size());

        let cols = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Min(50), Constraint::Max(30)].as_ref())
            .split(rows[2]);

        let sidebar = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Min(3), Constraint::Max(8), Constraint::Max(10)].as_ref())
            .split(cols[1]);

        let queues = app.queues();
        let process_states = app.states();

        Self::draw_header(frame, rows[0]);
        Self::draw_tabs(frame, rows[1], &process_states, self.active_process);
        Self::draw_memory(
            frame,
            cols[0],
            &process_states[self.active_process],
            &mut self.table_states[self.active_process],
            &mut self.scroll_states[self.active_process],
        );
        Self::draw_process_state(frame, sidebar[0], &process_states[self.active_process]);
        Self::draw_channels(frame, sidebar[1], &queues, self.active_process);
        Self::draw_talking_head(frame, sidebar[2]);
        Self::draw_help(frame, rows[3]);
    }

    fn draw_header(frame: &mut Frame, chunk: Rect) {
        let title_block = Block::default().style(
            Style::default()
                .fg(Monokai::Background.into())
                .bg(Monokai::Violet.into()),
        );

        let title = Paragraph::new("INTCODE COMPUTER")
            .block(title_block)
            .alignment(Alignment::Center);

        frame.render_widget(title, chunk);
    }

    fn draw_tabs(
        frame: &mut Frame<'_>,
        chunk: Rect,
        process_states: &[process::State],
        active_process: usize,
    ) {
        let block = Block::default()
            .title(Title::from("Processes").alignment(Alignment::Center))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Monokai::Yellow.into()))
            .border_type(BorderType::Rounded)
            .style(
                Style::default()
                    .fg(Monokai::White.into())
                    .bg(Monokai::Background.into()),
            );

        let tabs = process_states
            .iter()
            .enumerate()
            .map(|(i, state)| {
                let mut style = Style::default().bg(Monokai::Grey.into());
                if state.halted {
                    style = style.fg(Monokai::Red.into());
                } else if i == active_process {
                    style = style.fg(Monokai::White.into());
                }
                Span::from(format!("<   {}   >", i)).style(style)
            })
            .collect();
        let tabs = Tabs::new(tabs)
            .select(active_process)
            .block(block)
            .style(Style::default().fg(Monokai::DarkerGrey.into()))
            .highlight_style(Style::default().bg(Monokai::Green.into()));

        frame.render_widget(tabs, chunk);
    }

    fn draw_memory(
        frame: &mut Frame<'_>,
        chunk: Rect,
        process_state: &process::State,
        table_state: &mut TableState,
        scroll_state: &mut ScrollbarState,
    ) {
        let block = Block::default()
            .title(Title::from("Memory").alignment(Alignment::Center))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Monokai::Orange.into()))
            .border_type(BorderType::Rounded)
            .style(
                Style::default()
                    .fg(Monokai::White.into())
                    .bg(Monokai::Background.into()),
            );

        let (instruction, positions) = match process_state.next_instruction() {
            Some((instruction, _)) => (instruction, instruction.position_parameters()),
            None => (Instruction::Halt, Vec::new()),
        };

        let mut params_left = 0;

        let chunks: Vec<_> = process_state
            .memory
            .chunks(8)
            .enumerate()
            .map(|(i, chunk)| {
                let mut row = vec![Cell::from(format!("{:04}", i * 8))
                    .style(Style::default().bg(Monokai::DarkerGrey.into()))];
                for (j, v) in chunk.iter().enumerate() {
                    let mut style = Style::default().bg(Monokai::Background.into());
                    if process_state.instruction_pointer == i * 8 + j {
                        style = style.bg(Monokai::Green.into());
                        params_left = instruction.parameter_count();
                    } else if params_left > 0 {
                        style = style.bg(Monokai::Red.into());
                        params_left -= 1;
                    } else if positions.contains(&(i * 8 + j)) {
                        style = style.bg(Monokai::Blue.into());
                    }
                    row.push(Cell::from(format!("{}", v)).style(style));
                }
                Row::new(row)
            })
            .collect();
        let widths = [Constraint::Length(10); 9];
        let table = Table::new(chunks, widths)
            .block(block)
            .header(
                Row::new(vec![
                    Cell::from("Location"),
                    Cell::from("+0"),
                    Cell::from("+1"),
                    Cell::from("+2"),
                    Cell::from("+3"),
                    Cell::from("+4"),
                    Cell::from("+5"),
                    Cell::from("+6"),
                    Cell::from("+7"),
                ])
                .style(Style::default().bg(Monokai::DarkerGrey.into())),
            )
            .column_spacing(0);

        frame.render_stateful_widget(table, chunk, table_state);

        frame.render_stateful_widget(
            Scrollbar::default()
                .orientation(ScrollbarOrientation::VerticalRight)
                .begin_symbol(None)
                .end_symbol(None),
            chunk.inner(&ratatui::layout::Margin {
                horizontal: 1,
                vertical: 1,
            }),
            scroll_state,
        );
    }

    fn draw_process_state(frame: &mut Frame<'_>, chunk: Rect, process_state: &process::State) {
        let state_block = Block::default()
            .title(Title::from("State").alignment(Alignment::Center))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Monokai::Red.into()))
            .border_type(BorderType::Rounded)
            .style(
                Style::default()
                    .fg(Monokai::White.into())
                    .bg(Monokai::Background.into()),
            );

        let instruction = match process_state.next_instruction() {
            Some((instruction, _)) => instruction,
            None => Instruction::Halt,
        };

        let states = vec![
            format!("IP:          {:?}", process_state.instruction_pointer),
            format!("Halted:      {:?}", process_state.halted),
            format!("Last Input:  {:?}", process_state.last_input),
            format!("Last Output: {:?}", process_state.last_output),
            format!(""),
            format!("{}", instruction),
        ];

        let items = states.iter().map(Text::raw);
        let list = List::new(items).block(state_block);
        frame.render_widget(list, chunk);
    }

    fn draw_channels(
        frame: &mut Frame<'_>,
        chunk: Rect,
        channels: &[Vec<isize>],
        active_process: usize,
    ) {
        let block = Block::default()
            .title(Title::from("Channels").alignment(Alignment::Center))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Monokai::Violet.into()))
            .border_type(BorderType::Rounded)
            .style(
                Style::default()
                    .fg(Monokai::White.into())
                    .bg(Monokai::Background.into()),
            );

        let channels: Vec<_> = channels
            .iter()
            .enumerate()
            .map(|(i, channel)| {
                let mut style = Style::default().fg(Monokai::LightGrey.into());
                if i == active_process {
                    style = style.fg(Monokai::White.into());
                }
                Span::from(format!("{i}: {:?}", channel)).style(style)
            })
            .collect();

        let list = List::new(channels).block(block);
        frame.render_widget(list, chunk);
    }

    fn draw_talking_head(frame: &mut Frame, chunk: Rect) {
        let block = Block::default()
            .title(Title::from("Talking Head").alignment(Alignment::Center))
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Monokai::Blue.into()))
            .border_type(BorderType::Rounded)
            .style(
                Style::default()
                    .fg(Monokai::White.into())
                    .bg(Monokai::Background.into()),
            );

        frame.render_widget(block, chunk);
    }

    fn draw_help(frame: &mut Frame, chunk: Rect) {
        let block = Block::default().style(
            Style::default()
                .fg(Monokai::Background.into())
                .bg(Monokai::Green.into()),
        );

        let status = Paragraph::new("(q)uit | (s)tep | (0-4) select process")
            .block(block)
            .alignment(Alignment::Left);

        frame.render_widget(status, chunk);
    }
}
