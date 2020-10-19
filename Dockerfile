# -- Dockerfile --
# 这个文件负责构建包含你的程序的 Docker 容器

# 使用 Java 12
FROM openjdk:11
# 向容器内复制文件
COPY ./* /app/
# 编译程序
WORKDIR /app/
RUN javac Main.java
