# Create Tasks list
tasks <- 100
task_list <- data.frame(task = paste("Task", seq.int(1,to = tasks,  by = 1)),
                        task_depth = sample(x = c('A','B','C','D'), size = 100, replace = TRUE))

# Create Resource list
parts <- 50
parts_list <-  paste0("Part-", seq.int(1,to = parts,  by = 1))

tools <- 20
tools_list <-  paste0("Tool-", seq.int(1,to = tools,  by = 1))

skills <- 10
skills_list <-  paste0("Skill-", seq.int(1,to = skills,  by = 1))


#  Link Tasks and Resources

task_resources <- NULL
set.seed(31052020)
for (task in task_list$task) {

  # pick items
  pick_list <- data.frame(task = task, 
                          resource_type = 'part',
                          resource = unique(sample(x = parts_list, size = sample(1:10,1), replace = TRUE)))
  # add pick list to the data
  task_resources <- rbind(task_resources, pick_list)
  
  
  # pick tools
  pick_list <- data.frame(task = task, 
                          resource_type = 'tool',
                          resource = unique(sample(x = tools_list, size = sample(1:5,1), replace = TRUE)))
  # add pick list to the data
  task_resources <- rbind(task_resources, pick_list)
  
  # pick skills
  pick_list <- data.frame(task = task, 
                          resource_type = 'skill',
                          resource = unique(sample(x = skills_list, size = sample(1:5,3), replace = TRUE)))
  
  # add pick list to the data
  task_resources <- rbind(task_resources, pick_list)
  
}

# So now we have tasks and resources linked

# make heat map


