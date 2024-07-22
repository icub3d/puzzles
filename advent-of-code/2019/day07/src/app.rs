use crate::{
    ipc::Channel,
    process::{Process, State},
};

use std::sync::{Arc, Mutex};

use anyhow::Result;
use tokio::sync::mpsc::{self, Sender};

pub struct App {
    channels: Vec<Channel>,
    states: Vec<Arc<Mutex<State>>>,
    notifiers: Vec<Sender<()>>,
}

impl App {
    pub async fn new(input: &'static str) -> Result<Self> {
        let permutation = [0, 1, 2, 3, 4];
        let (channel, mut sender, mut receiver) = Channel::new(false);
        let first = sender.clone();
        let mut channels = vec![channel];
        let mut states = Vec::new();
        let mut notifiers = Vec::new();

        for (i, p) in permutation.iter().enumerate() {
            // Send messages and create the new channel.
            sender.send(*p as isize + 5).await?;
            if i == 0 {
                sender.send(0).await?;
            }
            let (channel, new_sender, new_receiver) = Channel::new(false);
            let new_sender = if i == 4 { first.clone() } else { new_sender };
            if i != 4 {
                channels.push(channel);
            }
            // Create our process and save our state
            let mut process =
                Process::new_with_program_and_input(input, receiver, new_sender.clone());
            let state = Arc::new(Mutex::new(process.state()));
            states.push(state.clone());

            // Create our notifier.
            let (notifier, mut notifier_receiver) = mpsc::channel::<()>(32);
            notifiers.push(notifier);

            tokio::spawn(async move {
                while notifier_receiver.recv().await.is_some() {
                    if process.state().halted {
                        break;
                    }
                    process.step().await.unwrap();
                    *state.lock().unwrap() = process.state();
                }
            });
            (sender, receiver) = (new_sender, new_receiver);
        }

        Ok(Self {
            channels,
            states,
            notifiers,
        })
    }

    pub async fn step(&self, index: usize) -> Result<()> {
        if self.states[index].lock().unwrap().halted {
            return Ok(());
        }
        self.notifiers[index].send(()).await?;
        Ok(())
    }

    pub fn queues(&self) -> Vec<Vec<isize>> {
        self.channels.iter().map(|c| c.queue()).collect()
    }

    pub fn states(&self) -> Vec<State> {
        self.states
            .iter()
            .map(|s| s.lock().unwrap().clone())
            .collect()
    }
}
