package cn.migu.file.core;

import java.util.regex.Pattern;

public class Pace {

	private String collectPath;

	private Pattern pattern;

	private String character;

	private String aid;

	private String sid;

	private String jid;

	private String oid;

	private String kind;

	private String decollator;

	private int colNum;

	private int line;

	private String extFunName;

	public Pace(String collectPath, Pattern pattern, String character, String aid, String sid, String jid, String oid,
			String kind, String decollator, int colNum, int line, String extFunName) {
		super();
		this.collectPath = collectPath;
		this.pattern = pattern;
		this.character = character;
		this.aid = aid;
		this.sid = sid;
		this.jid = jid;
		this.oid = oid;
		this.kind = kind;
		this.decollator = decollator;
		this.colNum = colNum;
		this.line = line;
		this.extFunName = extFunName;
	}

	@Override
	public String toString() {
		return "Pace [collectPath=" + collectPath + ", pattern=" + pattern + ", character=" + character + ", aid=" + aid
				+ ", sid=" + sid + ", jid=" + jid + ", oid=" + oid + ", kind=" + kind + ", decollator=" + decollator
				+ ", colNum=" + colNum + ", line=" + line + ", extFunName=" + extFunName + "]";
	}

	public String getExtFunName() {
		return extFunName;
	}

	public void setExtFunName(String extFunName) {
		this.extFunName = extFunName;
	}

	public int getLine() {
		return line;
	}

	public void setLine(int line) {
		this.line = line;
	}

	public int getColNum() {
		return colNum;
	}

	public void setColNum(int colNum) {
		this.colNum = colNum;
	}

	public String getAid() {
		return aid;
	}

	public void setAid(String aid) {
		this.aid = aid;
	}

	public String getSid() {
		return sid;
	}

	public void setSid(String sid) {
		this.sid = sid;
	}

	public String getJid() {
		return jid;
	}

	public void setJid(String jid) {
		this.jid = jid;
	}

	public String getOid() {
		return oid;
	}

	public void setOid(String oid) {
		this.oid = oid;
	}

	public String getCharacter() {
		return character;
	}

	public void setCharacter(String character) {
		this.character = character;
	}

	public Pattern getPattern() {
		return pattern;
	}

	public void setPattern(Pattern pattern) {
		this.pattern = pattern;
	}

	public String getCollectPath() {
		return collectPath;
	}

	public void setCollectPath(String collectPath) {
		this.collectPath = collectPath;
	}

	public String getKind() {
		return kind;
	}

	public void setKind(String kind) {
		this.kind = kind;
	}

	public String getDecollator() {
		return decollator;
	}

	public void setDecollator(String decollator) {
		this.decollator = decollator;
	}

}
