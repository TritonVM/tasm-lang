use std::collections::{HashMap, HashSet};

use crate::{
    ast::LetStmt,
    cfg::{self, BasicBlock, ControlFlowGraph, Variable},
};

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct SsaControlFlowGraph<'a> {
    pub entrypoint: &'a BasicBlock,
    pub edges: Vec<AnnotatedEdge<'a>>,
    pub nodes: Vec<BasicBlock>,
    pub exitpoint: &'a BasicBlock,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct AnnotatedEdge<'a> {
    source: &'a BasicBlock,
    destination: &'a BasicBlock,
    annotation: Option<Variable>,
}

pub fn convert(cfg: ControlFlowGraph) -> ControlFlowGraph {
    let mut visited: HashSet<usize> = HashSet::new();
    let mut active_set: Vec<(usize, &BasicBlock)> =
        vec![(cfg.entrypoint, &cfg.nodes[cfg.entrypoint])];
    let mut new_basic_blocks: Vec<BasicBlock> = vec![];
    while !active_set.is_empty() {
        let mut successors = vec![];

        // visit members of active set
        for (index, member) in active_set {
            // duplicate objects with bookkeeping
            let is_joining = cfg.edges.iter().filter(|e| e.destination == index).count() > 1;
            if is_joining {
                // let old_predecessors = cfg.edges
            }
            let parameterized_member = BasicBlock {
                index: member.index,
                stmts: member.stmts.clone(),
                param: member.param.clone(),
            };
            new_basic_blocks.push(parameterized_member);

            // mark visited
            visited.insert(index);

            // collect successors
            for successor in cfg
                .edges
                .iter()
                .filter(|e| e.source == index)
                .map(|e| e.destination)
            {
                successors.push(successor);
            }
        }

        // prepare for next iteration:
        // prune successor set and assign to active set
        active_set = successors
            .into_iter()
            .filter(|s| !visited.contains(s))
            .map(|i| (i, &cfg.nodes[i]))
            .collect();
    }

    todo!()
}
