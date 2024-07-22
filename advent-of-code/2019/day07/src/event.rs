use std::time::Duration;

use anyhow::{anyhow, Result};
use crossterm::event::{EventStream, KeyEvent, MouseEvent};
use futures::{FutureExt, StreamExt};
use tokio::sync::mpsc::{self, UnboundedReceiver};

#[derive(Debug, Copy, Clone)]
pub enum Event {
    Tick,
    Key(KeyEvent),
    Mouse(MouseEvent),
}

pub struct EventHandler {
    rx: UnboundedReceiver<Event>,
}

impl EventHandler {
    pub fn new(tick_rate: Duration) -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        let _tx = tx.clone();

        // TODO: pass handler to close on exit?
        let _handler = tokio::spawn(async move {
            let tx = _tx;
            let mut reader = EventStream::new();
            let mut tick = tokio::time::interval(tick_rate);
            loop {
                let tick_delay = tick.tick();
                let event = reader.next().fuse();
                tokio::select! {
                    _ = tick_delay => {
                        tx.send(Event::Tick).unwrap();
                    }
                    event = event => {
                        if let Some(Ok(event)) = event {
                            match event {
                                crossterm::event::Event::Key(key) => {
                                    tx.send(Event::Key(key)).unwrap();
                                }
                                crossterm::event::Event::Mouse(mouse) => {
                                    tx.send(Event::Mouse(mouse)).unwrap();
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        });
        Self { rx }
    }

    pub async fn next(&mut self) -> Result<Event> {
        self.rx.recv().await.ok_or(anyhow!("no event"))
    }
}
