provider "aws" {
  region = "us-west-2"
}

resource "aws_security_group" "radiofx-tg-bot-access" {
  name = "radiofx-tg-bot-access"

  ingress {
    description = "SSH"
    from_port = 22
    to_port = 22
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port = 0
    to_port = 0
    protocol = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_ecs_cluster" "radiofx-tg-bot" {
  name = "radiofx-tg-bot"
}

resource "aws_ecs_service" "radiofx-tg-bot" {
  name = "radiofx-tg-bot-server"
  cluster = aws_ecs_cluster.radiofx-tg-bot.id
  task_definition = aws_ecs_task_definition.radiofx-tg-bot.arn
  desired_count = 1
  launch_type = "EC2"
}

data "template_file" "task-definition" {
  template = "${file("radiofx-tg-bot-service.json")}"
  vars = {
    repository_url = "${aws_ecr_repository.radiofx-tg-bot.repository_url}"
  }
}

resource "aws_ecs_task_definition" "radiofx-tg-bot" {
  family = "radiofx-tg-bot-service"
  container_definitions = data.template_file.task-definition.rendered
}

resource "aws_ecr_repository" "radiofx-tg-bot" {
  name = "radiofx-tg-bot"
}

resource "aws_instance" "radiofx-tg-bot" {
  ami = "ami-0d6621c01e8c2de2c"
  instance_type = "t2.micro"
  key_name = "aleksxor-msi"
  vpc_security_group_ids = [
    "${aws_security_group.radiofx-tg-bot-access.id}"
  ]

  tags = {
    Name = "radiofx-tg-bot"
  }

  iam_instance_profile = "ecs-instance-role"

  user_data = <<-EOF
    #!/bin/bash
    hostname ${aws_ecs_cluster.radiofx-tg-bot.name}-server
    echo ECS_CLUSTER=${aws_ecs_cluster.radiofx-tg-bot.name} \
      >> /etc/ecs/ecs.config
    echo ECS_ENGINE_TASK_CLEANUP_WAIT_DURATION='30m' \
      >> /etc/ecs/ecs.config
  EOF
}
