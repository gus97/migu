package com.gus.service;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.BatchPreparedStatementSetter;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


@RestController
@RequestMapping("/mydev")
public class HttpService4BatchUpdate {

	@Autowired
	private JdbcTemplate jdbcTemplate;

	@Autowired
	private HttpServletRequest request;

	//http://127.0.0.1:8081/mydev/batchUpdate?thread_intterupt_id=100
	@RequestMapping("/batchUpdate")
	public String batchUpdate() {
		//单线程stop方式
		ThreadHandleUtils.SinglThreadHandle(request);
		
		//业务处理
		String sql = "insert into t1 values (?,?)";

		List<Foo> list = new ArrayList<Foo>();

		//批量插入1000000
		for (int i = 0; i < 1000000; i++) {

			list.add(new Foo(i, "插" + i));
		}

		jdbcTemplate.batchUpdate(sql, new BatchPreparedStatementSetter() {
			public int getBatchSize() {
				return list.size();
			}

			public void setValues(PreparedStatement ps, int i) throws SQLException {
				Foo foo = (Foo) list.get(i);
				ps.setInt(1, foo.getId());
				ps.setString(2, foo.getName());
			}
		});

		return "0000";
	}

}
