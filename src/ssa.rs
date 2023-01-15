use std::collections::{HashMap, HashSet};

use crate::cfg;

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct AnnotatedControlFlowGraph {
    pub entrypoint: usize,
    pub edges: Vec<AnnotatedEdge>,
    pub nodes: Vec<cfg::BasicBlock>,
    pub exitpoint: usize,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct AnnotatedEdge {
    source: usize,
    destination: usize,
    annotation: Option<cfg::Variable>,
}

pub fn convert(cfg: cfg::ControlFlowGraph) -> AnnotatedControlFlowGraph {
    let mut acfg = annotate_edges(cfg);
    rename_variables(&mut acfg);
    acfg
}

fn annotate_edges(cfg: cfg::ControlFlowGraph) -> AnnotatedControlFlowGraph {
    let mut visited: HashSet<usize> = HashSet::new();
    let mut active_set: Vec<(usize, &cfg::BasicBlock)> =
        vec![(cfg.entrypoint, &cfg.nodes[cfg.entrypoint])];
    let mut annotated_edges: Vec<AnnotatedEdge> = vec![];

    while !active_set.is_empty() {
        let mut successors = vec![];

        // visit members of active set
        for (member_index, member) in active_set {
            let annotation: Option<cfg::Variable> = match &member.last {
                cfg::Expr::Var(name) => Some(name.clone()),
                cfg::Expr::Lit(_) => None,
            };

            // find the outgoing edges for `member`
            // create outgoing edges annotated with `member.last`
            let mut annotated_outgoing_edges = cfg
                .edges
                .iter()
                .filter(|e| e.source == member_index)
                .map(|edge| AnnotatedEdge {
                    source: edge.source,
                    destination: edge.destination,
                    annotation: annotation.clone(),
                })
                .collect();

            annotated_edges.append(&mut annotated_outgoing_edges);

            // mark visited
            visited.insert(member_index);

            // collect successors
            for successor in cfg
                .edges
                .iter()
                .filter(|e| e.source == member_index)
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

    AnnotatedControlFlowGraph {
        entrypoint: cfg.entrypoint,
        edges: annotated_edges,
        nodes: cfg.nodes,
        exitpoint: cfg.exitpoint,
    }
}

fn rename_variables(cfg: &mut AnnotatedControlFlowGraph) {
    let mut visited: HashSet<usize> = HashSet::new();
    let mut active_set: Vec<usize> = vec![cfg.entrypoint];
    let mut renaming_dictionary = HashMap::<String, String>::new();
    let mut names_already_used: Vec<String> = vec![];

    let mut counter: usize = 0;
    let mut name_gen = |s: &str| {
        let tmp = format!("{}_{}", s, counter);
        counter += 1;
        tmp
    };

    while !active_set.is_empty() {
        let mut successors = vec![];

        // visit members of active set and rename each occurrence of
        // variables
        for member_index in active_set {
            let member = &mut cfg.nodes[member_index];

            // fi the basic block has a parameter
            if let Some(param) = &member.param {
                let data_type = param.data_type.to_owned();
                // a substitution exists for this name; apply it
                if let Some(new_name) = renaming_dictionary.get(&param.name) {
                    member.param = Some(cfg::Variable {
                        name: new_name.clone(),
                        data_type,
                    });
                    names_already_used.push(new_name.clone());
                }
                // this name was already used, get a new one
                else if names_already_used.contains(&param.name) {
                    let new_name = name_gen(&param.name);
                    renaming_dictionary.insert(param.name.clone(), new_name.clone());
                    member.param = Some(cfg::Variable {
                        name: new_name.clone(),
                        data_type,
                    });
                    names_already_used.push(new_name);
                }
                // first occurence
                else {
                    names_already_used.push(param.name.clone());
                }
            }

            // if a destination variable's name was seen, rename it and
            // all future occurrences of it.
            for let_stmt in member.stmts.iter_mut() {
                // a substitution exists for this name; apply it
                if let Some(new_name) = renaming_dictionary.get(&let_stmt.var.name) {
                    let_stmt.var.name = new_name.clone();
                    names_already_used.push(new_name.clone());
                }
                // this name was already used, get a new one
                else if names_already_used.contains(&let_stmt.var.name) {
                    let new_name = name_gen(&let_stmt.var.name);
                    renaming_dictionary.insert(let_stmt.var.name.clone(), new_name.clone());
                    names_already_used.push(let_stmt.var.name.clone());
                }
                // first occurence
                else {
                    names_already_used.push(let_stmt.var.name.clone());
                }

                rename_expr(&mut let_stmt.expr, &renaming_dictionary);
            }

            rename_expr(&mut member.last, &renaming_dictionary);

            // mark visited
            visited.insert(member_index);

            // collect successors
            for successor in cfg
                .edges
                .iter()
                .filter(|e| e.source == member_index)
                .map(|e| e.destination)
            {
                successors.push(successor);
            }
        }

        // prepare for next iteration:
        // prune successor set and assign to active set
        active_set = successors
            .into_iter()
            .filter(|successor| !visited.contains(successor))
            .collect();
    }
}

fn rename_variable(var: &mut cfg::Variable, renaming_dictionary: &HashMap<String, String>) {
    if let Some(new_name) = renaming_dictionary.get(&var.name) {
        let data_type = var.data_type.to_owned();
        *var = cfg::Variable {
            name: new_name.clone(),
            data_type,
        };
    }
}

fn rename_expr(expr: &mut cfg::Expr, renaming_dictionary: &HashMap<String, String>) {
    match expr {
        cfg::Expr::Var(var) => rename_variable(var, renaming_dictionary),
        cfg::Expr::Lit(_) => (),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{DataType, ExprLit},
        cfg::LetStmt,
    };

    use super::*;

    fn gen_cfg() -> cfg::ControlFlowGraph {
        // block_0():
        //   foo = 1
        //   call block_1(foo)
        //
        // block_1(foo):
        //   foo = 2
        //   foo = 3
        //   bar
        let foo_var = cfg::Variable {
            name: "foo".to_string(),
            data_type: DataType::U32,
        };

        let _bar_var = cfg::Variable {
            name: "bar".to_string(),
            data_type: DataType::U32,
        };

        let mut cfg = cfg::ControlFlowGraph::default();

        // block_0
        cfg.nodes.push(cfg::BasicBlock {
            index: 0,
            param: None,
            stmts: vec![LetStmt {
                var: foo_var.clone(),
                expr: cfg::Expr::Lit(ExprLit::CU32(1)),
            }],
            last: cfg::Expr::Var(foo_var.clone()),
        });

        // block_1
        cfg.nodes.push(cfg::BasicBlock {
            index: 1,
            param: Some(foo_var.clone()),
            stmts: vec![
                LetStmt {
                    var: foo_var.clone(),
                    expr: cfg::Expr::Lit(ExprLit::CU32(2)),
                },
                LetStmt {
                    var: foo_var.clone(),
                    expr: cfg::Expr::Lit(ExprLit::CU32(3)),
                },
            ],
            last: cfg::Expr::Var(foo_var),
        });

        // edges
        cfg.edges.push(cfg::Edge {
            source: 0,
            destination: 1,
        });

        cfg.exitpoint = 1;

        cfg
    }

    #[test]
    fn simple_ssa_test() {
        let cfg = gen_cfg();
        let mut acfg = annotate_edges(cfg);

        // todo: assert that annotations happened

        // println!("{:#?}", acfg);

        rename_variables(&mut acfg);

        println!("{:#?}", acfg);
    }
}
