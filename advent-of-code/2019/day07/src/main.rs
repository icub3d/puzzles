mod app;
mod event;
mod instruction;
mod ipc;
mod parameter;
mod process;
mod renderer;
mod tui;

use app::App;
use event::EventHandler;
use ipc::Channel;
use process::Process;
use tui::Tui;

use std::{io::stdout, time::Duration};

use anyhow::Result;
use clap::{command, Parser};
use ratatui::{backend::CrosstermBackend, Terminal};
use tokio::sync::mpsc::{self, Sender};

// Create a flag so we can run the tui.
#[derive(Parser)]
#[command(author, about, version)]
struct Cli {
    #[arg(short, long)]
    tui: bool,
}

#[tokio::main]
async fn main() -> Result<()> {
    let input = include_str!("../input");

    let args = Cli::parse();

    if args.tui {
        run_tui(input).await
    } else {
        part1(input).await?;
        part2(input).await
    }
}

async fn part1_run_permutation(
    input: &'static str,
    permutation: &[usize],
    output: Sender<isize>,
) -> Result<()> {
    let mut amplifiers = Vec::new();
    let (_, mut sender, mut receiver) = Channel::new(true);
    let mut first = sender.clone();

    for p in permutation.iter() {
        sender.send(*p as isize).await?;
        let (_, new_sender, new_receiver) = Channel::new(true);
        amplifiers.push(Process::new_with_program_and_input(
            input,
            receiver,
            new_sender.clone(),
        ));
        (sender, receiver) = (new_sender, new_receiver);
    }

    first.send(0).await?;
    for mut computer in amplifiers {
        match computer.run().await {
            Ok(_) => {}
            Err(e) => {
                println!("error: {}", e);
                return Err(e);
            }
        }
    }

    match receiver.recv().await {
        Some(value) => Ok(output.send(value).await?),
        None => Err(anyhow::anyhow!("no value received")),
    }
}

async fn part1(input: &'static str) -> Result<()> {
    let (max_send, mut max_recv) = mpsc::channel(10);

    for permutation in icub3d_combinatorics::Permutation::new(5) {
        let sender = max_send.clone();
        tokio::spawn(async move { part1_run_permutation(input, &permutation, sender).await });
    }

    drop(max_send);
    let mut p1 = 0;
    while let Some(max) = max_recv.recv().await {
        p1 = p1.max(max);
    }
    println!("p1: {}", p1);
    Ok(())
}

async fn part2_run_permutation(
    input: &'static str,
    permutation: &[usize],
    output: Sender<isize>,
) -> Result<()> {
    let (_, mut sender, mut receiver) = Channel::new(true);
    let first = sender.clone();

    for (i, p) in permutation.iter().enumerate() {
        sender.send(*p as isize + 5).await?;
        if i == 0 {
            sender.send(0).await?;
        }
        let (_, new_sender, new_receiver) = Channel::new(true);
        let new_sender = if i == 4 { first.clone() } else { new_sender };
        let my_sender = new_sender.clone();
        let output = output.clone();
        tokio::spawn(async move {
            let mut amplifier = Process::new_with_program_and_input(input, receiver, my_sender);
            amplifier.run().await.unwrap();
            if i == 0 {
                output
                    .send(amplifier.channel_receiver.recv().await.unwrap())
                    .await
                    .unwrap();
            }
        });
        (sender, receiver) = (new_sender, new_receiver);
    }
    Ok(())
}

async fn part2(input: &'static str) -> Result<()> {
    let (max_send, mut max_recv) = mpsc::channel(10);
    for permutation in icub3d_combinatorics::Permutation::new(5) {
        let sender = max_send.clone();
        tokio::spawn(async move { part2_run_permutation(input, &permutation, sender).await });
    }

    drop(max_send);
    let mut max = 0;
    while let Some(value) = max_recv.recv().await {
        max = max.max(value);
    }
    println!("p2: {}", max);
    Ok(())
}

async fn run_tui(input: &'static str) -> Result<()> {
    let app = App::new(input).await?;
    let backend = CrosstermBackend::new(stdout());
    let terminal = Terminal::new(backend)?;
    let mut events = EventHandler::new(Duration::from_millis(16));
    let mut tui = Tui::new(terminal, app).await;
    tui.init()?;

    while tui.running {
        tui.draw()?;
        let event = events.next().await?;
        tui.handle_event(event).await?;
    }

    tui.exit()
}
